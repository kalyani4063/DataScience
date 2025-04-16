data("mtcars")
mtcars
2. Define the Target Variable: Treat mpg as the dependent variable.
targetCol <- mtcars$mpg
3. Handle Categorical Variables: Convert categorical features (vs, am, gear, carb)
into numeric format
as.factor(mtcars$am)
Name: Kalyani S Roll No: 22CSEB11 Page No:
as.numeric(as.factor(mtcars$am))
mtcars$vs <- as.numeric(as.factor(mtcars$vs))
mtcars$am <- as.numeric(as.factor(mtcars$am))
mtcars$gear <- as.numeric(as.factor(mtcars$gear))
mtcars$carb <- as.numeric(as.factor(mtcars$carb))
4. Compute Correlations:
1. Use Pearson correlation (method = "pearson") to measure linear
relationships.
2. Use Spearman correlation (method = "spearman") to analyze ranked
relationships.
plot(mtcars)
pearson_cor <- cor(mtcars, method = "pearson")
spearman_cor <- cor(mtcars, method = "spearman")
plot(pearson_cor)
plot(spearman_cor)
corrplot(pearson_cor, method = "color", type = "upper", tl.cex = 0.8)
corrplot(spearman_cor, method = "color", type = "upper", tl.cex = 0.8)
5. Identify Important Features:
1. Determine which features have a strong positive correlation with mpg.
2. Identify features with a strong negative correlation with mpg.
3. List features that show no significant correlation with mpg.
4. Set a threshold (e.g., |correlation| > 0.5) to classify important features.
6. Visualize Correlation Results Using Multiple Plots:
1. Correlation Heatmap (ggcorrplot)
library(ggcorrplot)
ggcorrplot(corrValues, type ='lower', lab=TRUE, title = "Correlation Heatmap")
2. Scatter Plot with Regression Line (ggplot2)
library(ggplot2)
ggplot(corrValues,aes(x=wt, y=mpg))+geom_point()+geom_smooth(method = 'lm', se =
FALSE)+labs(title = "MPG vs Weight with Regression Line", x = "Weight", y = "MPG")
3. Box Plot (ggplot2)
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
ggplot(mtcars, aes(x = am, y = mpg, fill = am)) +
 geom_boxplot() +
 labs(title = "MPG by Transmission Type", x = "Transmission", y = "MPG")
4. Spearman vs Pearson Correlation Bar Plot (ggplot2)
pearson_corr_mpg <- pearson_cor["mpg", ]
 spearman_corr_mpg <- spearman_cor["mpg", ]
 corr_df <- data.frame(
 Variable = names(pearson_corr_mpg),
 Pearson = as.numeric(pearson_corr_mpg),
 Spearman = as.numeric(spearman_corr_mpg)
 )
 corr_long <- melt(corr_df, id.vars = "Variable", variable.name = "CorrelationType",
value.name = "Correlation")
 ggplot(corr_long, aes(x = Variable, y = Correlation, fill = CorrelationType)) +
 geom_bar(stat = "identity", position = "dodge") +
 coord_flip()
 labs(title = "Pearson vs Spearman Correlation with MPG", x = "Variable", y =
"Correlation")
5. Hierarchical Clustering Dendrogram (hclust)
dist_matrix <- as.dist(1 - abs(corrValues))
hc <- hclust(dist_matrix, method = "complete")
plot(hc, main = "Hierarchical Clustering Dendrogram", sub = "", xlab = "", cex = 0.8)
