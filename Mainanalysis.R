# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
housing_data <- read.csv("C:/Users/nikun/Documents/DSRP-2024-Chenkai/house-prices-advanced-regression-techniques/train.csv")

# Calculate the median sale price for each neighborhood
median_prices <- housing_data %>%
  group_by(Neighborhood) %>%
  summarise(MedianPrice = median(SalePrice))

# Add a binary variable for above/below median price
housing_data <- housing_data %>%
  left_join(median_prices, by = "Neighborhood") %>%
  mutate(AboveMedian = ifelse(SalePrice > MedianPrice, 1, 0))

# Set the order for categorical variables
housing_data$ExterQual <- factor(housing_data$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
housing_data$ExterCond <- factor(housing_data$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
housing_data$PavedDrive <- factor(housing_data$PavedDrive, levels = c("N", "P", "Y"))
housing_data$Street <- factor(housing_data$Street, levels = c("Grvl", "Pave"))

# Identify and exclude categorical variables with single levels
valid_categorical_vars <- housing_data %>%
  select_if(is.factor) %>%
  summarise_all(~ n_distinct(.)) %>%
  gather(variable, n_levels) %>%
  filter(n_levels > 1) %>%
  pull(variable)

# Update the model formula to exclude single-level variables
model_formula <- paste("AboveMedian ~",
                       paste(valid_categorical_vars, collapse = " + "))

# Fit a logistic regression model
model <- glm(as.formula(model_formula), data = housing_data, family = "binomial")

# Summary of the model
summary(model)

# Function to create plots for each numerical exterior feature
create_numeric_plot <- function(feature, title) {
  ggplot(housing_data, aes_string(x = feature, y = "SalePrice", color = "as.factor(AboveMedian)")) +
    geom_point(alpha = 0.7) +
    labs(title = title, x = feature, y = "Sale Price", color = "Above Median") +
    scale_color_manual(values = c("0" = "#d73027", "1" = "#1a9850"), labels = c("0" = "Below Median", "1" = "Above Median")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Function to create plots for each categorical exterior feature
create_categorical_plot <- function(feature, title) {
  filtered_data <- housing_data %>%
    group_by(Neighborhood) %>%
    filter(n_distinct(.data[[feature]]) > 1)
  
  ggplot(filtered_data, aes_string(x = feature, fill = "as.factor(AboveMedian)")) +
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

# Create and display plots for numerical features
plot1 <- create_numeric_plot("WoodDeckSF", "Effect of Wood Deck Area on Sale Price")
plot2 <- create_numeric_plot("OpenPorchSF", "Effect of Open Porch Area on Sale Price")
plot3 <- create_numeric_plot("EnclosedPorch", "Effect of Enclosed Porch Area on Sale Price")
plot4 <- create_numeric_plot("X3SsnPorch", "Effect of Three Season Porch Area on Sale Price")
plot5 <- create_numeric_plot("ScreenPorch", "Effect of Screen Porch Area on Sale Price")
plot6 <- create_numeric_plot("PoolArea", "Effect of Pool Area on Sale Price")

# Create and display plots for categorical features
plot7 <- create_categorical_plot("ExterQual", "Effect of Exterior Quality on Probability of Selling Above Median Price by Neighborhood")
plot8 <- create_categorical_plot("ExterCond", "Effect of Exterior Condition on Probability of Selling Above Median Price by Neighborhood")
plot9 <- create_categorical_plot("RoofStyle", "Effect of Roof Style on Probability of Selling Above Median Price by Neighborhood")
plot10 <- create_categorical_plot("PavedDrive", "Effect of Paved Drive on Probability of Selling Above Median Price by Neighborhood")
plot11 <- create_categorical_plot("Street", "Effect of Street Type on Probability of Selling Above Median Price by Neighborhood")

# Print plots
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)
print(plot7)
print(plot8)
print(plot9)
print(plot10)
print(plot11)


# Chi-Square Test for ExterQual
chisq_test_exterqual <- chisq.test(table(housing_data$ExterQual, housing_data$AboveMedian))
print(chisq_test_exterqual)

# Chi-Square Test for ExterCond
chisq_test_extercond <- chisq.test(table(housing_data$ExterCond, housing_data$AboveMedian))
print(chisq_test_extercond)

# Chi-Square Test for RoofStyle
chisq_test_roofstyle <- chisq.test(table(housing_data$RoofStyle, housing_data$AboveMedian))
print(chisq_test_roofstyle)

# Chi-Square Test for PavedDrive
chisq_test_paveddrive <- chisq.test(table(housing_data$PavedDrive, housing_data$AboveMedian))
print(chisq_test_paveddrive)

# Chi-Square Test for Street
chisq_test_street <- chisq.test(table(housing_data$Street, housing_data$AboveMedian))
print(chisq_test_street)


# t-Test for WoodDeckSF
t_test_wooddecksf <- t.test(WoodDeckSF ~ AboveMedian, data = housing_data)
print(t_test_wooddecksf)

# t-Test for OpenPorchSF
t_test_openporchsf <- t.test(OpenPorchSF ~ AboveMedian, data = housing_data)
print(t_test_openporchsf)

# t-Test for EnclosedPorch
t_test_enclosedporch <- t.test(EnclosedPorch ~ AboveMedian, data = housing_data)
print(t_test_enclosedporch)

# t-Test for X3SsnPorch
t_test_x3ssnporch <- t.test(X3SsnPorch ~ AboveMedian, data = housing_data)
print(t_test_x3ssnporch)

# t-Test for ScreenPorch
t_test_screenporch <- t.test(ScreenPorch ~ AboveMedian, data = housing_data)
print(t_test_screenporch)

# t-Test for PoolArea
t_test_poolarea <- t.test(PoolArea ~ AboveMedian, data = housing_data)
print(t_test_poolarea)


# Correlation between WoodDeckSF and AboveMedian
cor_wooddecksf <- cor.test(housing_data$WoodDeckSF, housing_data$AboveMedian)
print(cor_wooddecksf)

# Correlation between OpenPorchSF and AboveMedian
cor_openporchsf <- cor.test(housing_data$OpenPorchSF, housing_data$AboveMedian)
print(cor_openporchsf)

# Correlation between EnclosedPorch and AboveMedian
cor_enclosedporch <- cor.test(housing_data$EnclosedPorch, housing_data$AboveMedian)
print(cor_enclosedporch)

# Correlation between X3SsnPorch and AboveMedian
cor_x3ssnporch <- cor.test(housing_data$X3SsnPorch, housing_data$AboveMedian)
print(cor_x3ssnporch)

# Correlation between ScreenPorch and AboveMedian
cor_screenporch <- cor.test(housing_data$ScreenPorch, housing_data$AboveMedian)
print(cor_screenporch)

# Correlation between PoolArea and AboveMedian
cor_poolarea <- cor.test(housing_data$PoolArea, housing_data$AboveMedian)
print(cor_poolarea)
