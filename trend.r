library(ggplot2)
sd <- read.csv("sales_data.csv")
summary(sd)
mean(sd$Sales)
1. Plot a scatter plot of Months vs. Sales to observe the trend. Use the ggplot2 package
for visualization.
scatter_plot <- ggplot(sd, aes(x = Month, y = Sales)) +
 geom_point(color = 'red', size = 3) +
 theme_minimal() +
 labs(title = "Sales Trend Over Months", x = "Month", y = "Sales")
print(scatter_plot)
2. Fit a linear regression model to analyze the trend. And Display the model summary
and interpret the coefficients.
model <- lm(Sales ~ Month, data = sd)
linear_plot <- ggplot(sd, aes(x = Month, y = Sales)) +
 geom_point(color = "blue", size = 3) +
 geom_smooth(method = "lm", color = "red", se = FALSE, formula = y ~ x) +
 theme_minimal() +
 labs(title = "Linear Regression: Sales vs. Month", x = "Month", y = "Sales")
print(linear_plot)
summary(model)
summary(summary(model))
predict(model)
3. Calculate and interpret the R-squared value to assess the model’s performance.
r_square_value <- summary(model)$r.squared
cat("Linear Model R-squared:", r_square_value, "\n")
Linear Model R-squared: 0.9079635
4. Fit a quadratic regression model to check for a better fit.
sd$Month_squared <- sd$Month^2
quadratic_model <- lm(Sales ~ Month + Month_squared, data = sd)
quadratic_plot <- ggplot(sd, aes(x = Month, y = Sales)) +
 geom_point(color = "blue", size = 3) +
 geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "purple", se = FALSE) +
 theme_minimal() +
 labs(title = "Quadratic Regression: Sales vs. Month", x = "Month", y = "Sales")
print(quadratic_plot)
5. Compare the performance of linear vs. polynomial regression using R-squared and
RMSE (Root Mean Squared Error).
summary(quadratic_model)
quadratic_r_squared <- summary(quadratic_model)$r.squared
cat("Quadratic Model R-squared:", quadratic_r_squared, "\n")
rmse <- function(actual, predicted) {
 sqrt(mean((actual - predicted)^2))
}
linear_rmse <- rmse(sd$Sales, predict(model))
quadratic_rmse <- rmse(sd$Sales, predict(quadratic_model))
cat("Linear Model - R-squared:", r_square_value, " RMSE:", linear_rmse, "\n")
cat("Quadratic Model - R-squared:", quadratic_r_squared, " RMSE:", quadratic_rmse, "\n")
Linear Model - R-squared: 0.9079635 RMSE: 58.56041
Quadratic Model - R-squared: 0.9110244 RMSE: 57.57837
6. Identify which model better fits the dataset and explain why
if (quadratic_r_squared > r_square_value && quadratic_rmse < linear_rmse) {
 print("The quadratic model provides a better fit.")
} else {
 print("The linear model provides a better fit.")
}
[1] "The quadratic model provides a better fit."
7. Predict sales for the next 6 months (Months: 61 to 66).
pred_data <- data.frame(
 Month = 61:66,
 Month_squared = (61:66)^2
)
linear_predictions <- predict(model, pred_data)
quadratic_predictions <- predict(quadratic_model, pred_data)
pred_results <- data.frame(
 Month = 61:66,
 Linear_Predictions = linear_predictions,
 Quadratic_Predictions = quadratic_predictions
)
print(pred_results)
