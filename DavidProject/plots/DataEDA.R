# Exploratory Data Analysis (Over Time)

# SalePrice over time by OverallQual
ggplot(train_cleaned, aes(x = factor(YrSold), y = SalePrice, fill = factor(OverallQual))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Sale Price Over Time by Overall Quality", 
       x = "Year Sold", y = "Sale Price", fill = "Overall Quality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# SalePrice over time by OverallCond
ggplot(train_cleaned, aes(x = factor(YrSold), y = SalePrice, fill = factor(OverallCond))) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Sale Price Over Time by Overall Condition", 
       x = "Year Sold", y = "Sale Price", fill = "Overall Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# SalePrice over time by ExterQual
ggplot(train_cleaned, aes(x = factor(YrSold), y = SalePrice, fill = ExterQual)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Sale Price Over Time by Exterior Quality", 
       x = "Year Sold", y = "Sale Price", fill = "Exterior Quality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# SalePrice over time by ExterCond
ggplot(train_cleaned, aes(x = factor(YrSold), y = SalePrice, fill = ExterCond)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Sale Price Over Time by Exterior Condition", 
       x = "Year Sold", y = "Sale Price", fill = "Exterior Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
