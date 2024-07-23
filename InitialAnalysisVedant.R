library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(readr)

data <- read_csv("train.csv")
summary(data)
str(data)
data <- arrange(new_data, desc(SalePrice))
View(data)

ggplot(data, aes(x = Neighborhood, y = SalePrice)) +
  geom_bar(stat = 'identity') +
  labs(title = "Neighborhood vs Sale Price",
       x = "Neighborhood",
       y = "Sale Price")

ntp <- subset(data[, c("Neighborhood", "SalePrice")])

head(ntp, 5)

View(ntp)

average_prices <- aggregate(SalePrice ~ Neighborhood, data = ntp, FUN = mean)

average_prices

ggplot(average_prices, aes(x = Neighborhood, y = SalePrice)) +
  geom_bar(stat = 'identity') +
  labs(title = "Neighborhood vs Average Sale Price",
       x = "Neighborhood",
       y = "Sale Price")

