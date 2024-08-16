
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(readr)

train_housing <- read.csv("DavidProject/data/house-prices-advanced-regression-techniques/train.csv")
test_housing <- read.csv("DavidProject/data/house-prices-advanced-regression-techniques/test.csv")

colnames(train_housing)[colnames(train_housing) == "MSSubClass"] <- "DwellType"
colnames(train_housing)[colnames(train_housing) == "MSZoning"] <- "ZoneClass"
colnames(test_housing)[colnames(test_housing) == "MSSubClass"] <- "DwellType"
colnames(test_housing)[colnames(test_housing) == "MSZoning"] <- "ZoneClass"

# Add a column to distinguish between train and test datasets
train_housing$Dataset <- "train"
test_housing$Dataset <- "test"

combined_housing <- bind_rows(train_housing, test_housing)

selected_columns <- c("YrSold", "DwellType", "ZoneClass", "LotArea", "OverallQual", "Dataset")
combined_housing <- combined_housing |>
  select(all_of(selected_columns)) |>
  arrange(YrSold)

# Removing rows with NA in critical columns
combined_housing <- combined_housing |>
  filter(!is.na(OverallQual) & !is.na(ZoneClass) & !is.na(LotArea) & !is.na(DwellType))

# Convert categorical variables to factors
combined_housing <- combined_housing |>
  mutate(DwellType = as.factor(DwellType), ZoneClass = as.factor(ZoneClass), OverallQual = as.numeric(OverallQual), LotArea = as.numeric(LotArea))

combined_housing$ZoneClass <- droplevels(combined_housing$ZoneClass)

# Remove the top 10 observations of LotArea
combined_housing <- combined_housing |> arrange(desc(LotArea)) |> slice(-1:-10)

# Split the combined dataset back into train and test sets
train_cleaned <- combined_housing |> filter(Dataset == "train")
test_cleaned <- combined_housing |> filter(Dataset == "test")

dwell_type_labels <- c(
    `20` = "A",
    `30` = "B",
    `40` = "C",
    `45` = "D",
    `50` = "E",
    `60` = "F",
    `70` = "G",
    `75` = "H",
    `80` = "I",
    `85` = "J",
    `90` = "K",
    `120` = "L",
    `160` = "M",
    `180` = "N",
    `190` = "O"
  )

summary(train_cleaned)
str(train_cleaned)
summary(test_cleaned)
str(test_cleaned)