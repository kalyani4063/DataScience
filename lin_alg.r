library(ggplot2)
data <- read.csv("customer_purchase_behavior.csv")
data_scaled <- scale(data)
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
eigenvalues <- pca_result$sdev^2
variance_explained <- eigenvalues / sum(eigenvalues)
cumulative_variance <- cumsum(variance_explained)
print(data.frame(PC = 1:length(eigenvalues),
 Eigenvalues = eigenvalues,
 Variance_Explained = variance_explained,
 Cumulative_Variance = cumulative_variance))
num_components <- which(cumulative_variance >= 0.90)[1]
cat("Number of components explaining at least 90% variance:", num_components, "\n")
plot(cumulative_variance, type = "b",
 xlab = "Number of Principal Components",
 ylab = "Cumulative Variance Explained",
 main = "Scree Plot for PCA")
Name: Kalyani S Roll no:22CSEB11 Page no:
if (num_components <= 5) {
 reduced_data <- data.frame(pca_result$x[, 1:num_components])
 print("Dataset reduced to 5 dimensions.")
} else {
 print("More than 5 components needed for 90% variance.")
}
Outputs:
Question 2 : Correlation Analysis
A medical researcher is studying the relationship between different blood test parameters
(e.g., glucose levels, cholesterol, blood pressure) to identify risk factors for diabetes.
• Compute the Pearson and Spearman correlation coefficients for a given dataset of blood
test results.
• Explain how correlation matrices are used in feature selection and why high correlation
between two features might lead to redundancy.
• Using linear algebra, describe how the correlation matrix can be decomposed using
eigenvalues and eigenvectors.
Program:
library(ggplot2)
library(corrplot)
data <- read.csv("blood_test_parameters.csv")
pearson_corr <- cor(data, method = "pearson")
spearman_corr <- cor(data, method = "spearman")
print("Pearson Correlation Matrix:")
print(pearson_corr)
print("Spearman Correlation Matrix:")
print(spearman_corr)
corrplot(pearson_corr, method = "color", tl.cex = 0.8, addCoef.col = "black", number.cex =
0.7, title = "Pearson Correlation")
corrplot(spearman_corr, method = "color", tl.cex = 0.8, addCoef.col = "black", number.cex =
0.7, title = "Spearman Correlation")
eigen_decomp <- eigen(pearson_corr)
eigenvalues <- eigen_decomp$values
print("Eigenvalues of the correlation matrix:")
print(eigenvalues)
eigenvectors <- eigen_decomp$vectors
print("Eigenvectors of the correlation matrix:")
print(eigenvectors)
variance_explained <- eigenvalues / sum(eigenvalues)
cumulative_variance <- cumsum(variance_explained)
par(mar = c(5, 5, 4, 2)) # Set margins: (bottom, left, top, right)
plot(cumulative_variance, type = "b", pch = 19, col = "blue",
 xlab = "Number of Principal Components",
 ylab = "Cumulative Variance Explained",
 main = "Eigenvalue Decomposition",
 cex.lab = 1.2, cex.axis = 1.1, cex.main = 1.3) # Adjust text size
Outputs:
Question 3: Regression Analysis
Areal estate company wants to predict house prices based on factors such as square footage,
number of bedrooms, and location.
• Formulate the problem as a multiple linear regression equation and express it in matrix
form.
• Using the normal equation calculate the regression coefficients.
• Discuss how multicollinearity affects the regression model and how Principal
Component Regression (PCR) can mitigate it.
Program:
if (!require("car")) install.packages("car", dependencies = TRUE)
if (!require("pls")) install.packages("pls", dependencies = TRUE)
Name: Kalyani S Roll no:22CSEB11 Page no:
library(car)
library(pls)
data <- read.csv("real_estate_prices.csv", stringsAsFactors = FALSE)
str(data)
data$House_Price <- as.numeric(as.character(data$House_Price))
data$Square_Feet <- as.numeric(as.character(data$Square_Feet))
data$Bedrooms <- as.numeric(as.character(data$Bedrooms))
data$Location_Score <- as.numeric(as.character(data$Location_Score))
data <- na.omit(data[, c("House_Price", "Square_Feet", "Bedrooms", "Location_Score")])
if (nrow(data) == 0) {
 stop("Error: Dataset is empty after removing missing values!")
}
Y <- matrix(data$House_Price, ncol = 1)
X <- as.matrix(cbind(1, data$Square_Feet, data$Bedrooms, data$Location_Score))
print(dim(X)) # Should be (n, p)
print(dim(Y)) # Should be (n, 1)
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
print("Regression Coefficients:")
print(beta_hat)
lm_model <- lm(House_Price ~ Square_Feet + Bedrooms + Location_Score, data = data)
vif_values <- vif(lm_model)
print("Variance Inflation Factor (VIF):")
print(vif_values)
pcr_model <- pcr(House_Price ~ Square_Feet + Bedrooms + Location_Score,
 data = data, scale = TRUE, validation = "CV")
summary(pcr_model)
validationplot(pcr_model, val.type = "MSEP", main = "PCR Validation Plot")
