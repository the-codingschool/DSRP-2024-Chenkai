library(dplyr)
library(ggplot2)

# Load the dataset
housing_data <- read.csv("C:/Users/nikun/Documents/DSRP-2024-Chenkai/NikunjProject/house-prices-advanced-regression-techniques/train.csv")

# Convert categorical variables to factors with ordered levels
housing_data$ExterQual <- factor(housing_data$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
housing_data$ExterCond <- factor(housing_data$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
housing_data$LandSlope <- factor(housing_data$LandSlope, levels = c("Sev", "Mod", "Gtl"))


# Calculate the median sale price for each neighborhood
median_prices <- housing_data %>%
  group_by(Neighborhood) %>%
  summarise(MedianPrice = median(SalePrice, na.rm = TRUE))

# Add a binary variable for above/below median price
housing_data <- housing_data %>%
  left_join(median_prices, by = "Neighborhood") %>%
  mutate(AboveMedian = ifelse(SalePrice > MedianPrice, 1, 0))

# Function to filter neighborhoods with only one type of a feature
filter_single_type <- function(data, feature) {
  counts <- data %>%
    group_by(Neighborhood, !!sym(feature)) %>%
    tally() %>%
    group_by(Neighborhood) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    select(Neighborhood)
  filtered_data <- data %>%
    filter(Neighborhood %in% counts$Neighborhood)
  return(filtered_data)
}

# Create filtered datasets for each feature
filtered_data_ExterQual <- filter_single_type(housing_data, "ExterQual")
filtered_data_ExterCond <- filter_single_type(housing_data, "ExterCond")
filtered_data_RoofStyle <- filter_single_type(housing_data, "RoofStyle")
filtered_data_PavedDrive <- filter_single_type(housing_data, "PavedDrive")
filtered_data_Street <- filter_single_type(housing_data, "Street")
filtered_data_LandSlope <- filter_single_type(housing_data, "LandSlope")
filtered_data_Foundation <- filter_single_type(housing_data, "Foundation")


# Logistic regression analysis function
run_single_feature_logistic_regression <- function(data, feature) {
  formula <- as.formula(paste("AboveMedian ~", feature))
  model <- glm(formula, data = data, family = "binomial")
  return(summary(model))
}

# Create plots for categorical features
create_categorical_plot <- function(data, feature, title) {
  ggplot(data, aes_string(x = feature, fill = "as.factor(AboveMedian)")) +
    geom_bar(position = "fill") +
    facet_wrap(~ Neighborhood) +
    labs(title = title, x = feature, fill = "Above Median") +
    scale_fill_manual(values = c("0" = "#d73027", "1" = "#1a9850"), labels = c("0" = "Below Median", "1" = "Above Median")) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_discrete(drop = FALSE)
}

# Run logistic regression for each feature separately
summary_ExterQual <- run_single_feature_logistic_regression(filtered_data_ExterQual, "ExterQual")
summary_ExterCond <- run_single_feature_logistic_regression(filtered_data_ExterCond, "ExterCond")
summary_RoofStyle <- run_single_feature_logistic_regression(filtered_data_RoofStyle, "RoofStyle")
summary_PavedDrive <- run_single_feature_logistic_regression(filtered_data_PavedDrive, "PavedDrive")
summary_Street <- run_single_feature_logistic_regression(filtered_data_Street, "Street")
summary_LandSlope <- run_single_feature_logistic_regression(filtered_data_LandSlope, "LandSlope")
summary_Foundation <- run_single_feature_logistic_regression(filtered_data_Foundation, "Foundation")

# Create plots for the new features
plot_ExterQual <- create_categorical_plot(filtered_data_ExterQual, "ExterQual", "Effect of Exterior Quality on Probability of Selling Above Median Price by Neighborhood")
plot_ExterCond <- create_categorical_plot(filtered_data_ExterCond, "ExterCond", "Effect of Exterior Condition on Probability of Selling Above Median Price by Neighborhood")
plot_RoofStyle <- create_categorical_plot(filtered_data_RoofStyle, "RoofStyle", "Effect of Roof Style on Probability of Selling Above Median Price by Neighborhood")
plot_PavedDrive <- create_categorical_plot(filtered_data_PavedDrive, "PavedDrive", "Effect of Paved Drive on Probability of Selling Above Median Price by Neighborhood")
plot_Street <- create_categorical_plot(filtered_data_Street, "Street", "Effect of Street Type on Probability of Selling Above Median Price by Neighborhood")
plot_LandSlope <- create_categorical_plot(filtered_data_LandSlope, "LandSlope", "Effect of Land Slope on Probability of Selling Above Median Price by Neighborhood")
plot_Foundation <- create_categorical_plot(filtered_data_Foundation, "Foundation", "Effect of Foundation Type on Probability of Selling Above Median Price by Neighborhood")


# Print summaries and plots
print(summary_ExterQual)
print(summary_ExterCond)
print(summary_RoofStyle)
print(summary_PavedDrive)
print(summary_Street)
print(summary_LandSlope)
print(summary_Foundation)


print(plot_ExterQual)
print(plot_ExterCond)
print(plot_RoofStyle)
print(plot_PavedDrive)
print(plot_Street)
print(plot_LandSlope)
print(plot_Foundation)

