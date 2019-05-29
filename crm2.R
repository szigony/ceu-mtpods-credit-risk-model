# Libraries
library(dplyr)
library(data.table)

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

# Drop variables
repayment <- repayment %>% 
  dplyr::select(-c(first_month, month_to_use))

# Keep last month data
repayment <- repayment %>% 
  filter(month <= "2008-02-29" & status < 5)

# Customer data
data <- inner_join(repayment, customer, by = "ID")

# Add customer age
data <- data %>% 
  mutate(customer_age = month_difference(paste(
    substr(data$birth_date, 7, 10),
    substr(data$birth_date, 1, 2),
    substr(data$birth_date, 4, 5),
    sep = "-"), month) / 12
  )

# Add flag for used cars
data <- data %>% 
  mutate(car_used = ifelse(car_age < 12, 0, 1))

# Brands
brands_to_keep <- readRDS("brands_to_keep.rds")
data <- data %>% 
  mutate(brand = ifelse(brand %in% brands_to_keep, brand, "OTHER"))

# Convert non-numeric columns to factor
data <- data %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_each(list(as.factor), default) %>% 
  mutate_each(list(as.factor), car_used)

# Load the model
model <- readRDS("credit_model.rds")
data_with_probabilites <- cbind(data, predict(model, data, type = "response"))

# Capital data
capital_data <- fread("data/capital.csv")
data_with_capitals <- inner_join(data_with_probabilites, capital_data) %>% 
  mutate(required_provision = score * capital)

# This is the number for which the bank should provision based on the clients defaulting in the next month
sum(data_with_capitals$required_provision)