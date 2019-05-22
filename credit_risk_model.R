# Libraries
library(data.table)
library(zoo)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(MASS)
library(pROC)

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
  select(-c("first_month", "last_month", "month_to_use", "month_to_use_target", "max_status_future"))

# Remove records
repayment <- repayment %>% 
  filter(month <= "2007-08-31" | default == 1)
