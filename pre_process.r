#1.loading data and show few rows
data<- read.csv("titanic.csv")
head(data)
#2.data exploration
str(data)
summary(data)
head(data)
Kalyani S 22cseb11
#identify categorical columns
categorical_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
cat("Categorical Columns:", names(data)[categorical_cols], "\n")
# Identify numerical columns
numerical_cols <- sapply(data, is.numeric)
cat("Numerical Columns:", names(data)[numerical_cols], "\n")
#missing data
missing<- is.na(data)
#missing data visualization
barplot(missing,
 main = "visualization of missing values",
 xlab = "Columns",
 ylab = "Count of Missing Values",
 col = "skyblue",
 border = "blue")
Kalyani S 22cseb11
a
#handling missing values
#cols with missing values
colSums(is.na(data))
#replacing missing values
filled<-data #copy is created
#numeric
isnumeric<-sapply(filled, is.numeric)
filled[isnumeric] <- lapply(filled[isnumeric], function(x) {
 x[is.na(x)] <- mean(x, na.rm = TRUE)
 return(x)
})
Kalyani S 22cseb11
#categorical data
isword<-sapply(filled, is.factor)
filled[isword] <- lapply(filled[isword], function(x) {
 x[is.na(x)] <- "unknown"
 return(x)
})
#transforming categorical DATA
filled$Sex <- as.factor(filled$Sex)
filled$Embarked <- as.factor(filled$Embarked)
filled$Pclass <- as.factor(filled$Pclass)
# View updated structure
str(filled)
Kalyani S 22cseb11
a
#one-hot encoding = present -> 1 absent ->0
#using model.matrix
encoded_data <- model.matrix(~ Pclass + Embarked - 1, data = data)
#Display encoded data
print(encoded_data)
Kalyani S 22cseb11
install.packages("caret")
install.packages("ggplot2")
library(ggplot2)
library(caret)
# Create dummy variables
dummy_model <- dummyVars(~ Pclass + Embarked, data = data)
encoded_data <- predict(dummy_model, newdata = data)
# Display encoded data
print(encoded_data)
Kalyani S 22cseb11
a
#how does the dummyvars works?
 #Each unique level of the categorical variable becomes a new column.
#The original categorical variable is replaced with binary (0 or 1) columns.
#SCALING NUMERICAL FEATURES
filled$Age <- scale(filled$Age)
filled$Fare <- scale(filled$Fare)
Kalyani S 22cseb11
# Check the scaled data
head(filled[, c("Age", "Fare")])
sd(filled$Age)
sd(filled$Fare)
#FEATURE ENGINEERING
# Assuming 'data' is your dataframe
# 1. Create AgeGroup feature
filled$AgeGroup <- ifelse(filled$Age < 18, "Child",
 ifelse(filled$Age >= 18 & filled$Age < 60, "Adult", "Senior"))
# 2. Create FamilySize feature
filled$FamilySize <- filled$SibSp + filled$Parch + 1
# View the new features
head(filled[, c("Age", "AgeGroup", "SibSp", "Parch", "FamilySize")])
Kalyani S 22cseb11
a
# SAVE the cleaned data into a new CSV file
write.csv(filled, file = "cleaned_titanic.csv", row.names = FALSE)
newdata<-read.csv("cleaned_titanic.csv")
#VISUALIZATION
install.packages("esquisse")
install.packages("shiny")
install.packages("ggplot2")
library(ggplot2)
library(esquisse)
Kalyani S 22cseb11
library(ggplot2)
#TASK1
ggplot(newdata) +
aes(x = Age) +
geom_histogram(bins = 30L, fill = "#C13C84") +
labs(title = "HISTOGRAM FOR AGE") +
theme_gray() +
theme(plot.title = element_text(face = "italic", hjust = 0.5), legend.text = element_text(face =
"italic"),
legend.title = element_text(face = "bold"))
ggplot(newdata) +
 aes(x = Age) +
 geom_density(fill = "#CC0ED5") +
 labs(title = "DENSITY PLOT FOR AGE ") +
 theme_minimal() +
Kalyani S 22cseb11
 theme(
 plot.title = element_text(face = "italic",
 hjust = 0.5)
 )
#task2
ggplot(newdata) +
 aes(x = Sex, y = Survived) +
 geom_col(fill = "#BF5061") +
 labs(
 title = "Bar plot comparing the survival rate for male and female passengers."
 ) +
 theme_minimal() +
 theme(
Kalyani S 22cseb11
 plot.title = element_text(face = "italic",
 hjust = 0.5)
 )
#TASK3
esquisser(data = newdata )
ggplot(newdata) +
 aes(x = Pclass, y = Fare) +
 geom_point(colour = "#c13") +
 labs(
 x = "PCLASS",
 y = "FARE",
 title = "RELATIONSHIP BETWEEN PCLASS AND FARE"
Kalyani S 22cseb11
 ) +
 theme_minimal() +
 theme(
 plot.title = element_text(face = "italic",
 hjust = 0.5),
 axis.title.y = element_text(face = "italic"),
 axis.title.x = element_text(face = "italic")
 )
#task 4
ggplot(newdata) +
 aes(x = Age, y = Fare) +
 geom_point(colour = "#15AD15") +
 labs(
 x = "AGE",
 y = "FARE",
 title = "RELATIONSHIP BETWEEN AGE AND FARE"
 ) +
 theme_minimal() +
 theme(
 plot.title = element_text(face = "italic",
 hjust = 0.5),
 axis.title.y = element_text(face = "italic"),
 axis.title.x = element_text(face = "italic")
 )
#TASK 5
ggplot(newdata) +
 aes(x = Embarked) +
 geom_bar(fill = "#CA114F") +
 labs(
 x = "EMBARKED",
 y = "COUNT",
 title = "BARPLOT FOR EMBARKED"
 ) +
 theme_minimal() +
 theme(
 plot.title = element_text(face = "italic",
 hjust = 0.5),
 axis.title.y = element_text(face = "italic"),
 axis.title.x = element_text(face = "italic")
 )
#WITH CHANGE IN THEME
ggplot(newdata) +
 aes(x = Embarked) +
 geom_bar(fill = "#C8083D") +
 labs(
 x = "EMBARKED",
 y = "COUNT",
 title = "BARPLOT FOR EMBARKED"
 ) +
 theme_linedraw() +
 theme(
 plot.title = element_text(face = "italic",
 hjust = 0.5),
 axis.title.y = element_text(face = "italic"),
 axis.title.x = element_text(face = "italic")
 )
