1. What is the probability of randomly selecting a car from the mtcars dataset with a
mpg value greater than the upper bound of the IQR for mpg (i.e., an outlier)?
Q1 <- quantile(mtcars$mpg, 0.25)
Q3 <- quantile(mtcars$mpg, 0.75)
IQR <- Q3 - Q1
upper_bound <- Q3 + 1.5 * IQR
outlier_count <- sum(mtcars$mpg > upper_bound)
total_cars <- nrow(mtcars)
probability <- outlier_count / total_cars
2. Given that a car is an outlier in terms of horsepower (hp), what is the conditional
probability that it also has an outlier value for weight (wt)?
Q1_hp <- quantile(mtcars$hp, 0.25)
Q3_hp <- quantile(mtcars$hp, 0.75)
IQR_hp <- Q3_hp - Q1_hp
upper_bound_hp <- Q3_hp + 1.5 * IQR_hp
Q1_wt <- quantile(mtcars$wt, 0.25)
Q3_wt <- quantile(mtcars$wt, 0.75)
IQR_wt <- Q3_wt - Q1_wt
upper_bound_wt <- Q3_wt + 1.5 * IQR_wt
outliers_hp <- mtcars$hp > upper_bound_hp
outliers_wt <- mtcars$wt > upper_bound_wt
outliers_hp_count <- sum(outliers_hp)
outliers_both_count <- sum(outliers_hp & outliers_wt)
conditional_probability <- ifelse(outliers_hp_count > 0, outliers_both_count /
outliers_hp_count, 0)
cat("Conditional Probability:", conditional_probability, "\n")
3. If you randomly select two cars from the mtcars dataset, what is the probability that
both are outliers in terms of mpg using the Z-score method?
z_mpg <- (mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg)
outliers_mpg <- abs(z_mpg) > 1.96
p_outlier <- sum(outliers_mpg) / length(mtcars$mpg)
p_both_outliers <- p_outlier * p_outlier
cat("Probability of selecting two mpg outliers:", p_both_outliers, "\n")
4. What is the probability that a randomly chosen car from the dataset has a Z-score for
hp greater than 2 or less than -2 (indicating an outlier)?
z_hp <- (mtcars$hp - mean(mtcars$hp)) / sd(mtcars$hp)
outliers_hp <- abs(z_hp) > 2
p_outlier_hp <- sum(outliers_hp) / length(mtcars$hp)
cat("Probability of selecting an hp outlier:", p_outlier_hp, "\n")
5. Assuming a normal distribution for mpg, what is the probability that a randomly
selected car falls within 1 standard deviation from the mean mpg?
Q1_mpg <- quantile(mtcars$mpg, 0.25)
Q3_mpg <- quantile(mtcars$mpg, 0.75)
IQR_mpg <- Q3_mpg - Q1_mpg
Q1_wt <- quantile(mtcars$wt, 0.25)
Name: Kalyani S Roll No: 22CSEB11 Page No:
Q3_wt <- quantile(mtcars$wt, 0.75)
IQR_wt <- Q3_wt - Q1_wt
non_outliers_mpg <- mtcars$mpg >= Q1_mpg & mtcars$mpg <= Q3_mpg
non_outliers_wt <- mtcars$wt >= Q1_wt & mtcars$wt <= Q3_wt
p_non_outliers <- sum(non_outliers_mpg & non_outliers_wt) / nrow(mtcars)
cat("Probability of mpg and wt within IQR:", p_non_outliers, "\n")
6. What is the probability that a randomly selected car has both an mpg and wt within
the interquartile range (i.e., not an outlier for either variable)?
p_mpg <- 0.15
p_hp <- 0.10
p_both <- p_mpg * p_hp
cat("Probability of being an outlier in both mpg and hp:", p_both, "\n")
7. If the probability of a car being an outlier in mpg is 0.15 and in hp is 0.10, assuming
independence, what is the probability that a car is an outlier in both mpg and hp?
p_mpg <- 0.15
p_hp <- 0.10
p_both <- p_mpg * p_hp
cat("Probability of being an outlier in both mpg and hp:", p_both, "\n")
8. What is the expected number of outliers in the mtcars dataset if the probability of any
car being an outlier (based on Z-score > 2 or < -2) is 0.05?
p_any_outlier <- 0.05
expected_outliers <- p_any_outlier * nrow(mtcars)
cat("Expected number of outliers in the dataset:", expected_outliers, "\n")
Name: Kalyani S Roll No: 22CSEB11 Page No:
9. Given that a car has a high Z-score in mpg, what is the probability that it also has an
above-average hp?
mean_hp <- mean(mtcars$hp)
z_mpg <- (mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg)
high_mpg <- z_mpg > 1
above_avg_hp <- mtcars$hp > mean_hp
p_hp_given_mpg <- sum(high_mpg & above_avg_hp) / sum(high_mpg)
cat("Probability that a high-mpg car has above-average hp:", p_hp_given_mpg, "\n")
10. What is the shape of the distribution for the variables (e.g., mpg, horsepower,
weight), and do they show skewness or multimodality?
library(moments)
skew_mpg <- skewness(mtcars$mpg)
skew_hp <- skewness(mtcars$hp)
skew_wt <- skewness(mtcars$wt)
cat("Skewness of mpg:", skew_mpg, "\n")
cat("Skewness of hp:", skew_hp, "\n")
cat("Skewness of wt:", skew_wt, "\n")
par(mfrow = c(1,3)) # Arrange plots side by side
hist(mtcars$mpg, main="MPG Distribution", col="lightblue", breaks=10)
hist(mtcars$hp, main="HP Distribution", col="lightgreen", breaks=10)
hist(mtcars$wt, main="WT Distribution", col="lightcoral", breaks=10)
par(mfrow = c(1,1))
11. Are there any data points that significantly deviate from the expected range for any
variable that can be considered outliers?
z_hp <- (mtcars$hp - mean(mtcars$hp)) / sd(mtcars$hp)
z_mpg <- (mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg)
z_wt <- (mtcars$wt - mean(mtcars$wt)) / sd(mtcars$wt)
outliers_z_hp <- abs(z_hp) > 2
outliers_z_mpg <- abs(z_mpg) > 2
outliers_z_wt <- abs(z_wt) > 2
outliers_combined <- data.frame(
 hp_z = outliers_z_hp,
 mpg_z = outliers_z_mpg,
 wt_z = outliers_z_wt
)
cat("Outliers identified using Z-score and IQR methods:\n")
print(outliers_combined)
12. Which variable shows a stronger tendency for outliers, and do extreme values in one
variable correlate with unusual values in others?
Name: Kalyani S Roll No: 22CSEB11 Page No:
outliers_count <- colSums(outliers_combined)
cat("Outliers count for each variable:\n")
print(outliers_count)
cat("Comparison of Z-score vs IQR methods:\n")
cat("Z-score vs IQR for hp:", sum(outliers_z_hp), "vs", sum(outliers_iqr_hp), "\n")
cat("Z-score vs IQR for mpg:", sum(outliers_z_mpg), "vs", sum(outliers_iqr_mpg), "\n")
cat("Z-score vs IQR for wt:", sum(outliers_z_wt), "vs", sum(outliers_iqr_wt), "\n")
13. Based on boxplots, how many outliers can be identified for each variable, and what
do these visualizations suggest about their distributions?
par(mfrow = c(1, 3)) # Arrange plots side by side
boxplot(mtcars$hp, main="Boxplot of HP", col="lightblue")
boxplot(mtcars$mpg, main="Boxplot of MPG", col="lightgreen")
boxplot(mtcars$wt, main="Boxplot of WT", col="lightcoral")
par(mfrow = c(1, 1)) # Reset layout
14. Do different outlier detection methods (e.g., Z-score and IQR) identify the same or
different outliers, and why might the results differ?
common_outliers <- sum(outliers_z_hp & outliers_iqr_hp)
cat("Common outliers for HP:", common_outliers, "\n")
common_outliers_mpg <- sum(outliers_z_mpg & outliers_iqr_mpg)
cat("Common outliers for MPG:", common_outliers_mpg, "\n")
common_outliers_wt <- sum(outliers_z_wt & outliers_iqr_wt)
cat("Common outliers for WT:", common_outliers_wt, "\n")
15. How sensitive are the Z-score and IQR methods to the presence of extreme values in
the dataset?
mtcars_extreme <- mtcars
mtcars_extreme$hp[1] <- 500
z_hp_extreme <- (mtcars_extreme$hp - mean(mtcars_extreme$hp)) / sd(mtcars_extreme$hp)
outliers_z_hp_extreme <- abs(z_hp_extreme) > 2
Q1_hp_extreme <- quantile(mtcars_extreme$hp, 0.25)
Q3_hp_extreme <- quantile(mtcars_extreme$hp, 0.75)
IQR_hp_extreme <- Q3_hp_extreme - Q1_hp_extreme
upper_bound_hp_extreme <- Q3_hp_extreme + 1.5 * IQR_hp_extreme
lower_bound_hp_extreme <- Q1_hp_extreme - 1.5 * IQR_hp_extreme
outliers_iqr_hp_extreme <- mtcars_extreme$hp < lower_bound_hp_extreme |
mtcars_extreme$hp > upper_bound_hp_extreme
cat("Number of outliers using Z-score after adding extreme value:",
sum(outliers_z_hp_extreme), "\n")
cat("Number of outliers using IQR after adding extreme value:",
sum(outliers_iqr_hp_extreme), "\n")
16. Which data points are outliers based on horsepower, and do these align with
expected vehicle types or features (e.g., sports cars)?
outliers_hp_z <- abs(z_hp) > 2 # Z-score method
outliers_hp_iqr <- mtcars$hp < lower_bound_hp | mtcars$hp > upper_bound_hp # IQR
method
outliers_hp_data <- mtcars[outliers_hp_z | outliers_hp_iqr, ]
cat("Outliers based on horsepower:\n")
Name: Kalyani S Roll No: 22CSEB11 Page No:
print(outliers_hp_data)
17. Which data points are outliers based on weight, and how do these compare to typical
trends for lighter or heavier vehicles?
outliers_wt_z <- abs(z_wt) > 2 # Z-score method
outliers_wt_iqr <- mtcars$wt < lower_bound_wt | mtcars$wt > upper_bound_wt # IQR
method
outliers_wt_data <- mtcars[outliers_wt_z | outliers_wt_iqr, ]
cat("Outliers based on weight:\n")
print(outliers_wt_data)
18. What factors might explain why certain data points are outliers (e.g., vehicle type,
engine design, weight)?
outliers_hp_data$mpg
outliers_hp_data$wt
outliers_hp_data$am
19. Could any outliers be the result of data entry errors, and how can data cleaning
address such issues?
suspicious_data <- mtcars[mtcars$hp < 0 | mtcars$wt < 0, ]
cat("Suspicious data points with negative values:\n")
print(suspicious_data)
Name: Kalyani S Roll No: 22CSEB11 Page No:
mtcars_cleaned <- mtcars[!(mtcars$hp < 0 | mtcars$wt < 0), ]
20. Are there any variables where extreme values seem unreasonable, suggesting
possible errors in data collection or measurement?
cat("Minimum and maximum values for horsepower:\n")
print(range(mtcars$hp))
cat("Minimum and maximum values for weight:\n")
print(range(mtcars$wt))
cat("Minimum and maximum values for mpg:\n")
print(range(mtcars$mpg))
21. Do the detected outliers represent cars with either extreme fuel efficiency or high
performance, and what might this indicate about their design?
outliers_mpg_hp <- mtcars[abs(z_mpg) > 2 | abs(z_hp) > 2, ]
cat("Outliers in mpg and hp:\n")
print(outliers_mpg_hp)
outliers_mpg_hp$car_model
22. Are the outliers concentrated in particular categories (e.g., sports cars, luxury cars),
and what does this suggest about the performance characteristics of those categories?
table(outliers_mpg_hp$car_model)
23. How might the presence of outliers affect key statistical measures (e.g., mean,
standard deviation), and should they be removed for analysis?
mean_hp <- mean(mtcars$hp)
sd_hp <- sd(mtcars$hp)
mean_hp_no_outliers <- mean(mtcars[!outliers_hp_z, ]$hp)
sd_hp_no_outliers <- sd(mtcars[!outliers_hp_z, ]$hp)
cat("Mean and SD with outliers:\n")
cat(mean_hp, sd_hp, "\n")
cat("Mean and SD without outliers:\n")
cat(mean_hp_no_outliers, sd_hp_no_outliers, "\n")
24. How might the presence of outliers impact the accuracy and interpretation of
regression models, and should robust regression techniques be used?
library(MASS)
robust_model <- rlm(mpg ~ hp + wt, data = mtcars)
summary(robust_model)
25. If the dataset represents a company’s car fleet, how might identifying outliers in
performance metrics (e.g., fuel efficiency, horsepower) influence decision-making?
outliers_fuel_efficiency <- mtcars[outliers_mpg_z, ]
Name: Kalyani S Roll No: 22CSEB11 Page No:
outliers_performance <- mtcars[outliers_hp_z, ]
cat("Outliers in fuel efficiency:\n")
print(outliers_fuel_efficiency)
cat("Outliers in performance:\n")
print(outliers_performance)
26. How do outliers in the data reflect unique vehicle performance characteristics, and
what insights can be drawn for automotive design?
Outliers in horsepower (hp) may indicate high-performance vehicles, while outliers in mpg
suggest fuel-efficient designs. These reflect trade-offs between performance and economy in
vehicle design.
27. What role do outliers play in understanding real-world trends for vehicle
performance, and how should they be handled in subsequent analyses?
Outliers highlight exceptional vehicle performance (e.g., high hp or mpg), influencing market
segmentation. They should be analyzed, not discarded, and handled with robust techniques in
subsequent analyses.
28. Are there any notable patterns or relationships between outliers and other variables,
and how could these influence future data collection strategies?
Outliers in hp often correlate with low mpg and high weight, revealing performance vs.
efficiency trade-offs. Future data collection should include detailed categories and focus on
accurate data entry for deeper insights.
plot(mtcars$hp, mtcars$mpg, main="Horsepower vs MPG", xlab="Horsepower",
ylab="MPG")
points(mtcars[outliers_hp_z, ]$hp, mtcars[outliers_hp_z, ]$mpg, col="red", pch=19) #
Outliers in red
plot(mtcars$hp, mtcars$wt, main="Horsepower vs Weight", xlab="Horsepower",
ylab="Weight")
points(mtcars[outliers_hp_z, ]$hp, mtcars[outliers_hp_z, ]$wt, col="red", pch=19) # Outliers
in red
