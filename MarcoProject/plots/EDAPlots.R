#install necessary packages
install.packages("ggplot2")
library(ggplot2)

#read in datasets
data <- read.csv("data/train.csv")

data_small <- select(data, YearBuilt, LotArea, GrLivArea, BedroomAbvGr, FullBath, OverallQual, OverallCond, BldgType, GarageQual, SalePrice)
arrange(data_small, SalePrice)

#average price per neighborhood
ggplot(data = avg_neigh, aes(x = Neighborhood, y = price)) +
  geom_col()

#scatter plot of living space vs sale price
ggplot(data = data_small, aes(x = GrLivArea, y = SalePrice)) +
  geom_point()

ggplot(data = data_small, aes(x = YearBuilt, y = SalePrice)) +
  geom_point()

#scatter plot of lot area vs sale price
ggplot(data = data_small, aes(x = LotArea, y = SalePrice)) +
  geom_point()

#box plot of neighborhood vs price
ggplot(data = data_small, aes(x = factor(Neighborhood), y = SalePrice)) +
  geom_boxplot()

ggplot(data = data_small, aes(x = factor(OverallCond), y = GrLivArea)) +
  geom_boxplot()

#distribution of houses by year built
ggplot(data = data_small, aes(YearBuilt)) +
  geom_histogram(bins = 139)

#box plot of zoning vs price
ggplot(data = avg_zone, aes(x=factor(MSZoning), y = price)) +
  geom_col()

#box plot of condition vs price
ggplot(data = data_small, aes(x=factor(OverallCond), y = SalePrice)) +
  geom_boxplot() +
  labs(title = "Average Sale Price by Condition",
       x = "Condition",
       y = "Sale Price")

#distributions of houses by condition
ggplot(data = data_small, aes(OverallCond)) +
  geom_histogram(bins = 9)

#distributions of houses by quality
ggplot(data = data_small, aes(OverallQual)) +
  geom_histogram(bins = 10)

#box plot of sale price by quality
ggplot(data = data_small, aes(x=factor(OverallQual), y = SalePrice)) +
  geom_boxplot() +
  labs(title = "Average Sale Price by Quality",
       x = "Quality",
       y = "Average Sale Price")


#line plot of year built vs sale price
ggplot(data = data, aes(x = YearBuilt, y = SalePrice)) +
  geom_line(stat = "summary", fun = "mean") +
  scale_x_continuous(labels = label_comma()) +
  labs(title = "Average Sale Price of Houses Built Over Time",
       x = "Year Built",
       y = "Average Sale Price")

#double line plot of avg overall quality and condition vs year built
ggplot(data = data, aes(YearBuilt)) +
  geom_line(aes(y = OverallCond, colour = "OverallCond"), 
            stat = "summary", fun = "mean") +
  geom_line(aes(y = OverallQual, colour = "OverallQual"),
            stat = "summary", fun = "mean") +
  labs(title ="Average Quality and Condition of Houses by Year Built",
       x = "Year Built",
       y = "Rating") +
  theme(plot.title = element_text(size=11)) +
  scale_x_continuous(limits = c(1871, 2009))

#line plot of livable area vs year built
ggplot(data = data, aes(x = YearBuilt, y = GrLivArea)) +
  geom_line(stat = "summary", fun = "mean")

#line plot of lot area vs year built
ggplot(data, aes(x = YearBuilt, y = LotArea)) +
  geom_line(stat = "summary", fun = "mean")

#bar plot of distribution of garage types vs year built
ggplot(data, aes(x = GarageYrBlt, fill = GarageType)) +
  geom_bar(position = "fill") +
  labs(title = "Area of Garages Built Over Time",
       x = "Garage Construction Year",
       y = "Garage Area (sq. ft)",
       fill = "Garage Type")

ggplot(data, aes(x = YearRemodAdd, fill = GarageType)) +
  geom_bar(position = "fill") +
  labs(title = "Area of Garages Built Over Time",
       x = "Garage Construction Year",
       y = "Garage Area (sq. ft)",
       fill = "Garage Type")

data$BsmtQual <- factor(data$BsmtQual, levels = c("Ex", "Gd", "TA", "Fa", "NA"))
ggplot(filter(data, YearRemodAdd < YrSold), aes(x = YearRemodAdd, fill = BsmtQual)) +
  geom_bar(position = "fill")

ggplot(data, aes(x = YearBuilt, fill = BsmtQual)) +
  geom_histogram(position = "fill", bins = 139)

ggplot(data, aes(x = YearBuilt, fill = HouseStyle)) +
  geom_histogram(position = "fill", bins = 139)

ggplot(data, aes(x = YearBuilt, fill = KitchenQual)) +
  geom_histogram(position = "fill", bins = 139)

ggplot(data, aes(x = YearBuilt, fill = RoofStyle)) +
  geom_histogram(position = "fill", bins = 139)


  