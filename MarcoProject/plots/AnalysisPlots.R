library(ggplot2)

# accuracy vs knn hyperparameter (k value) line plot 2d knn
ggplot(data = knn_df, aes(x = k_val, y = accuracy, group = 1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0.6, 0.8)) +
  labs(title = 'Effect of K Value on KNN Accuracy (BsmtQual)',
       x = "K Value",
       y = "Accuracy")

# accuracy vs knn hyperparameter (k value) line plot 3d knn
ggplot(data = knn_df2, aes(x = k_val, y = accuracy, group = 1)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0.6, 0.8)) +
  labs(title = 'Effect of K Value on KNN Accuracy (BsmtQual)',
       x = "K Value",
       y = "Accuracy")

#multiple line plot
ggplot(knn_joined, aes(x = k_val, group = 1)) +
  geom_line(aes(y = BsmtQualAcc, colour = "BsmtQualAcc")) +
  geom_line(aes(y = HouseStyleAcc, colour = "HouseStyleAcc")) +
  geom_line(aes(y = KitchenQualAcc, colour = "KitchenQualAcc")) +
  labs(title ="Average Accuracy by K Value",
       x = "K Value",
       y = "Accuracy") +
  theme(plot.title = element_text(size=11))

#ttest results comparison (bar chart)
ggplot(knn_new, aes(x=factor(Feature), y = Accuracy)) +
  geom_col(stats = "summary", fun = "mean")
