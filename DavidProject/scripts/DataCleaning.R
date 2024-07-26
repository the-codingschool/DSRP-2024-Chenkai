# Setup and Data Cleaning

library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(readr)

train_housing <- read.csv("data/house-prices-advanced-regression-techniques/train.csv")
test_housing <- read.csv("data/house-prices-advanced-regression-techniques/test.csv")

# Add a column to distinguish between train and test datasets
train_housing$Dataset <- "Train"
test_housing$Dataset <- "Test"

# Add a placeholder for the target variable
test_housing$SalePrice <- NA

combined_housing <- bind_rows(train_housing, test_housing)
combined_housing
str(combined_housing)
summary(combined_housing)

selected_columns <- c("YrSold", "OverallQual", "OverallCond", "ExterQual", "ExterCond", "SalePrice", "Dataset")
combined_housing <- combined_housing |>
  select(all_of(selected_columns)) |>
  arrange(YrSold)

# Categorical variables to factors
combined_housing <- combined_housing |>
  mutate(across(where(is.character), as.factor))

# Split combined dataset back into train and test sets
train_cleaned <- combined_housing |> filter(Dataset == "Train")
test_cleaned <- combined_housing |> filter(Dataset == "Test")
train_cleaned
test_cleaned