# Libraries
library(data.table)
library(zoo)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(MASS)
library(pROC)
library(caret)
library(tidyr)

# Functions
month_since_reference <- function(d) {
  lt <- as.POSIXlt(d)
  lt$year * 12 + lt$mon
}

month_difference <- function(d1, d2) {
  month_since_reference(d2) - month_since_reference(d1)
}

# Load the data
customer <- fread("data/customer_data.csv")
repayment <- fread("data/repayment_data.csv")

# Data preparation
repayment <- repayment %>%
  group_by(ID) %>% 
  mutate(first_month = min(month)) %>% 
  ungroup() %>% 
  mutate(month_number = month_difference(first_month, month) + 1,
         month_to_use = pmin(month_number, 6)) %>% 
  arrange(ID, desc(month))

repayment <- repayment %>% 
  mutate(max_status_6m = rollapply(status, month_to_use, FUN = max, align = "left"))

repayment <- repayment %>% 
  mutate(delayed = max_delay > 0,
         nr_delays_6m = rollapply(delayed, month_to_use, FUN = sum, align = "left"),
         avg_delay_6m = rollapply(max_delay, month_to_use, FUN = mean, align = "left"),
         max_delay_6m = rollapply(max_delay, month_to_use, FUN = max, align = "left"))

# Target variable
repayment <- repayment %>% 
  group_by(ID) %>% 
  mutate(last_month = max(month)) %>%
  ungroup() %>% 
  arrange(ID, month) %>% 
  mutate(month_to_use_target = pmin(month_difference(month, last_month) + 1, 7),
         max_status_future = rollapply(status, month_to_use_target, FUN = max, align = "left"),
         default = ifelse(max_status_future == 5, 1, 0))

# Drop variables
repayment <- repayment %>% 
  dplyr::select(-c(first_month, last_month, month_to_use, month_to_use_target, max_status_future))

# Remove records
repayment <- repayment %>% 
  filter(month <= "2007-08-31" | default == 1)

# Add repayment over 6 months
repayment <- as.data.table(repayment %>% 
  mutate(repayment_month_over_6 = ifelse(month_number > 6, 1, 0)))

# Customer data
data <- inner_join(repayment, customer, by = "ID")

# Remove individual datasets and functions
rm(customer, repayment)

# Add customer age
data <- data %>% 
  mutate(customer_age = month_difference(paste(
    substr(data$birth_date, 7, 10),
    substr(data$birth_date, 1, 2),
    substr(data$birth_date, 4, 5),
    sep = "-"), month) / 12
  )

ggplot(data, aes(x = customer_age)) + geom_histogram()
ggplot(data, aes(x = car_age)) + geom_histogram()

# Add flag for used cars
data <- data %>% 
  mutate(car_used = ifelse(car_age < 12, 0, 1))

# Training - test partitions
ids <- unique(data$ID)
sample <- sort(sample(length(ids), length(ids) * 0.8)) # 80% for training
train_ids <- ids[sample] 

training_set <- data %>% filter(ID %in% train_ids)
test_set <- data %>% anti_join(training_set, by = "ID")

default_customer_ratio <- function(dt) {
  cr <- test_set %>% 
    group_by(ID) %>% 
    mutate(max_default = max(default))
  
  return(mean(cr$max_default))
}

default_customer_ratio(training_set)
default_customer_ratio(test_set)

# Brands
brands <- training_set %>% 
  group_by(brand) %>% 
  summarise(default_ratio = mean(default),
            count = n())

brands_to_keep <- brands %>% 
  filter(count > 10000) %>% 
  pull(brand)
saveRDS(brands_to_keep, "brands_to_keep.rds")

training_set <- training_set %>% 
  mutate(brand = ifelse(brand %in% brands_to_keep, brand, "OTHER"))

test_set <- test_set %>% 
  mutate(brand = ifelse(brand %in% brands_to_keep, brand, "OTHER"))

rcorr(
  as.matrix(
    training_set %>% 
      dplyr::select(default, max_delay, max_status_6m, max_delay_6m, month_number, delayed, customer_age,
                    car_age, car_used)
  )
)

colnames(training_set)
training_set <- training_set %>% 
  dplyr::select(default, max_delay, status, month_number, max_status_6m, nr_delays_6m, avg_delay_6m, max_delay_6m,
                repayment_month_over_6, debt_amount, own_resource, duration, payment_type, car_age, brand,
                engine_capacity_cat, residence_type, region, customer_age, car_used)

str(training_set)

training_set <- training_set %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_each(list(as.factor), default) %>% 
  mutate_each(list(as.factor), car_used)

test_set <- test_set %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_each(list(as.factor), default) %>% 
  mutate_each(list(as.factor), car_used) 

# Build a model
model <- glm(default ~ ., data = training_set, family = binomial(link = "logit"))
summary(model)

# Coefficients
coef(model)

# Variable importance
varimp <- varImp(model)

# Predictions
train_prediction <- cbind(training_set, score = predict(model, training_set, type = "response")) %>% 
  dplyr::select(default, score)

test_prediction <- cbind(test_set, score = predict(model, test_set, type = "response")) %>% 
  dplyr::select(default, score)

# ROC, AUC
plot(roc(train_prediction$default, train_prediction$score), print.auc = TRUE)
plot(roc(test_prediction$default, test_prediction$score), print.auc = TRUE)

# Kolmogorov-Smirnov index
ks_transform <- function(dt) {
  good <- dt %>% 
    filter(default == 0) %>% 
    dplyr::select(score)
  
  bad <- dt %>% 
    filter(default == 1) %>% 
    dplyr::select(score)
  
  good_score_counts <- good %>% 
    group_by(score) %>% 
    summarise(good_count = n())
  
  bad_score_counts <- bad %>% 
    group_by(score) %>% 
    summarise(bad_count = n())
  
  score_count <- full_join(good_score_counts, bad_score_counts, by = "score") %>% 
    arrange(score) %>% 
    mutate(good_count = replace_na(good_count, 0),
           bad_count = replace_na(bad_count, 0),
           good_cumsum = cumsum(good_count),
           bad_cumsum = cumsum(bad_count),
           good_ratio = good_cumsum / sum(good_count),
           bad_ratio = bad_cumsum / sum(bad_count),
           diff = good_ratio - bad_ratio)
    
  return(score_count)
}

ks_training <- ks_transform(train_prediction)
## TODO calculate cut-off
ggplot() +
  geom_line(data = ks_training, aes(x = score, y = good_ratio), color = "blue") +
  geom_line(data = ks_training, aes(x = score, y = bad_ratio), color = "red") +
  geom_vline(xintercept = cut_off)

ks_test <- ks_transform(test_prediction)
ggplot() +
  geom_line(data = ks_test, aes(x = score, y = good_ratio), color = "blue") +
  geom_line(data = ks_test, aes(x = score, y = bad_ratio), color = "red") +
  geom_vline(xintercept = cut_off)

# Lift chart
lift_values <- function(dt, nr) {
  
  dt <- dt %>% 
    mutate(default = as.numeric(default) - 1) %>% 
    arrange(-score)
  
  bin <- dim(dt)[1] / nr
  
  dt <- dt %>% 
    mutate(bin = floor(index(dt) / bin)) %>% 
    group_by(bin) %>% 
    mutate(default_sum = sum(default)) %>% 
    ungroup() %>% 
    mutate(default_cumsum = cumsum(default_sum),
           random_expected = sum(default) * (bin + 1) / nr,
           lift_value = default_cumsum / random_expected,
           contact_percent = bin * (100 / nr))
  
  return(dt)
  
}

lift_training <- lift_values(train_prediction, 20)

ggplot(data = lift_training, aes(x = contact_percent, y = lift_value, group = 1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue")

lift_test <- lift_values(test_prediction, 20)

ggplot(data = lift_test, aes(x = contact_percent, y = lift_value, group = 1)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "blue")

# ROI
transfer_roi <- function(dt) {
  
  roi_dt <- dt %>% 
    arrange(-score) %>% 
    mutate(cost = 100,
           potential_win = runif(dim(.)[1], 200, 1000),
           win = ifelse(default == 1, potential_win - cost, 0 - cost),
           roi = cumsum(win),
           percentile = 100 * as.numeric(rownames(.)) / dim(.)[1])
  
  return(roi_dt)
  
}

roi_train <- transfer_roi(train_prediction)

## TODO recalculate roi_optimal_score
roi_optimal_score <- roi_train %>% 
  group_by(score) %>% 
  summarise(max_roi = max(roi))

ggplot(data = roi_train, aes(x = score, y = roi, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +
  geom_vline(xintercept = roi_optimal_score$score)

ggplot(data = roi_train, aes(x = percentile, y = roi)) +
  geom_line() +
  geom_point()

roi_test <- transfer_roi(test_prediction)
ggplot(data = roi_test %>% sample_n(10000), aes(x = score, y = roi, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +
  geom_vline(xintercept = roi_optimal_score$score)

# Required provision
test_prediction <- test_prediction %>% 
  mutate(capital = runif(dim(.)[1], 200000, 10000000),
         req_provision = ifelse(default == 0, 0, capital),
         predicted_provision = capital * score)

sum(test_prediction$req_provision)
sum(test_prediction$predicted_provision)

100 * sum(test_prediction$predicted_provision) / sum(test_prediction$req_provision)

# Save the model
saveRDS(model, file = "credit_model.rds")
