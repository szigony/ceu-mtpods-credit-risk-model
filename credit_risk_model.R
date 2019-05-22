# Libraries
library(data.table)
library(zoo)
library(ggplot2)
# install.packages("Hmisc")
library(Hmisc)
# install.packages("MASS")
library(MASS)
# install.packages("pROC")
library(pROC)

# Load the data
customer <- read.csv("data/customer_data.csv", sep = ";")
repayment <- read.csv("data/repayment_data.csv", sep = ";")

