# Distribution of OverallQual
ggplot(train_cleaned, aes(x = factor(OverallQual))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Overall Quality", x = "Overall Quality", y = "Count") +
  theme_minimal()

# Histogram of DwellType
ggplot(train_cleaned, aes(x = DwellType)) +
  geom_bar(binwidth=5,fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of DwellType", x = "DwellType", y = "Count") +
  theme_minimal()

# Histogram of ZoneClass
ggplot(train_cleaned, aes(x = ZoneClass)) +
  geom_bar(fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of ZoneClass", x = "ZoneClass", y = "Count") +
  theme_minimal()

# Distribution of LotArea
ggplot(train_cleaned, aes(x = LotArea)) +
  geom_histogram(binwidth = 500, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Lot Area", x = "Lot Area", y = "Count") +
  theme_minimal()

# Filter LotArea for visualization of OverallQual and LotArea
train_filtered <- train_cleaned |> filter(LotArea <= 20000)

# Relationship between OverallQual and DwellType
ggplot(train_cleaned, aes(x = factor(DwellType), fill = factor(OverallQual))) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_x_discrete(labels = dwell_type_labels) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Overall Quality by DwellType", x = "DwellType", y = "Proportion", fill = "Overall Quality") +
  theme_minimal()

# Relationship between OverallQual and ZoneClass
ggplot(train_cleaned, aes(x = ZoneClass, y = OverallQual, fill = ZoneClass)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Overall Quality by ZoneClass", x = "ZoneClass", y = "Overall Quality") +
  theme_minimal()

# Relationship between OverallQual and LotArea (First iteration mosaic plot)
ggplot(train_filtered, aes(x = LotArea, fill = factor(OverallQual))) +
  geom_histogram(position = "fill", binwidth = 500, alpha = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Overall Quality by Lot Area", x = "Lot Area", y = "Proportion", fill = "Overall Quality") +
  theme_minimal()
