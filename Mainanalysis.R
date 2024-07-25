library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(readr)
library(forcats)


housing_data<-read_csv("C:/Users/nikun/Documents/DSRP-2024-Chenkai/house-prices-advanced-regression-techniques/train.csv")


median_prices <- housing_data %>%
  group_by(Neighborhood) %>%
  summarise(MedianPrice = median(SalePrice))

# Add a binary variable for above/below median price
housing_data <- housing_data %>%
  left_join(median_prices, by = "Neighborhood") %>%
  mutate(AboveMedian = ifelse(SalePrice > MedianPrice, 1, 0))# adds a table to tell if the house price was above the mediano of the neighborhood

housing_data$ExterQual <- factor(housing_data$ExterQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
housing_data$ExterQual <- factor(housing_data$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
# Fit a logistic regression model
model <- glm(AboveMedian ~ ExterQual + ExterCond + RoofStyle + Neighborhood, data = housing_data, family = "binomial")

# Summary of the model
summary(model)

# Visualize the effect of exterior features on the probability of selling above median price
ggplot(housing_data, aes(x = ExterQual, fill = as.factor(AboveMedian))) +
  geom_bar(position = "fill") +
  facet_wrap(~ Neighborhood) +
  labs(title = "Effect of Exterior Quality on Probability of Selling Above Median Price by Neighborhood",
       x = "Exterior Quality", fill = "Above Median") +
  scale_x_discrete(drop = FALSE)

# check for outliers nikunj 
# NridgHT , Swisu and BRKSIDE are like outliers

ggplot(housing_data, aes(x = ExterCond, fill = as.factor(AboveMedian))) +
  geom_bar(position = "fill") +
  facet_wrap(~ Neighborhood) +
  labs(title = "Effect of Exterior Condition on Probability of Selling Above Median Price by Neighborhood",
       x = "Exterior Condition", fill = "Above Median") +
  scale_x_discrete(drop = FALSE)

