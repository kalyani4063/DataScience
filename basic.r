1. Use R as a calculator to compute the mean of the numbers 15, 20, 35, 40, and 50.
values <- c(5, 20, 35, 40, 50);
cat("Mean of Values is : " , mean(values))
2. Assign the result of 102+5 to a variable and display it.
Indexing Vectors
result <- 102 + 5
cat("Result : ",result)
3. Create a numeric vector of 10 elements. Extract the 3rd and 7th elements.
myvector <- c(1,2,3,4,5,6,7,8,9,10)
cat("3rd Element :" , myvector[3] ," and 7th Element :",myvector[7] )
4. Replace the 5th element of a vector with 100.
myvector[5] <- 100
cat("Replaced 5th Element with 100 : ", myvector)
Vectorized Expressions
5. Create two vectors, x and y, each of length 5. Compute their element-wise sum and product.
vector1 <- c(1,2,3,4,5)
vector2 <- c(5,4,3,2,1)
vectorsum <- vector1 + vector2
print(vectorsum)
vectorprod <- vector1 * vector2
print(vectorprod)
6. Find all elements in a vector greater than a specific value (e.g., 10).
vector1 <- c(1,2,3,4,5)
print(vector1[vector1 > 3])
7. Write an R script that computes the mean of a vector with proper comments explaining each step.
vector1 <- c(1,2,3,4,5)
# using the mean function finding the mean of the vector created
print(mean(vector1))
Functions
8. Use the mean and sd functions to compute the mean and standard deviation of a dataset containing the
numbers 2, 4, 6, 8, 10.
ds <- c(2, 4, 6, 8, 10)
cat("Mean :",mean(ds))
cat("SD :",sd(ds))
9. Write a function to calculate the square of a number and test it with different inputs.
squareval <- function(x){return(x*x)}
cat("square of 2 :", squareval(2))
cat("square of 100 :", squareval(100))
Control Structures
10. Write if statement to check if a number is even or odd.
oddeven <- function(x) {
 if(x %% 2 == 0) {
 return("Even")
 } else {
 return("Odd")
 }
}
cat("The value 20 is :", oddeven(20), "\n")
cat("The value 11 is :", oddeven(11), "\n")
11. Use for loop to calculate the sum of squares of numbers from 1 to 10.
sumofsquares <- function(data) {
 sum = 0
 for(i in data) {
 sum = sum + i * i
 }
 return(sum)
}
cat("The sum of Square of 1,2,3 :",sumofsquares(c(1,2,3)))
Factors
12. Create a factor variable representing three types of fruits: "Apple," "Banana, "Cherry." Display the
levels of the factor
fruits <- factor(c("Apple", "Banana", "Cherry"))
print(fruits)
levels(fruits)
13. Convert a character vector to a factor and find the frequency of each level.
charvector <- c("a","b","c","d","a","c");
charfactor <- factor(charvector)
levels(charfactor)
print(table(charfactor))
Data Frames
14. Create a data frame with two columns: Age and Height for five individuals. Add a new column Weight.
df <- data.frame(
 Age = c(25, 30, 22, 28, 35),
 Height = c(160, 175, 168, 180, 165)
)
df$Weight <- c(55, 70, 60, 75, 65) # Weight column
print(df)
15. Extract all rows from the data frame where the Age is greater than 25.
print(df$Age[df$Age > 25])
Dealing with Missing Values
16. Create a vector containing some NA values. Use a function to replace all NAs with the mean of the
remaining values.
my_na_vector <- c(1, 2, NA, 4, NA, 6, 7, 8, 9, 10)
mean_value <- mean(my_na_vector, na.rm = TRUE)
my_na_vector[is.na(my_na_vector)] <- mean_value
print(my_na_vector)
17. Filter out rows with missing values in a data frame.
df_sal <- data.frame(
 Age = c(25, 30, 22, 28, NA),
 Salary = c(16000, 17500, 16800, NA, 16500)
)
is.na(df_sal)
print(na.omit(df_sal))
Using R Packages
18. Install and load the dplyr package. Use it to filter rows in a data frame where Age > 30.
install.packages("dplyr")
library("dplyr")
filter(df_sal, df_sal$Age > 25)
Data Pipelines
19. Using the magrittr package, write a pipeline that reads a vector, removes missing values, and computes
its mean.
library(magrittr)
my_vector <- c(1, 2, NA, 4, NA, 6, 7, 8, 9, 10)
mean_value <- my_vector %>% na.omit() %>% mean()
cat("The mean of the vector without NA values is:", mean_value)
20. Write a pipeline to filter rows from a data frame, calculate the mean of a column, and print the result.
mean_salary <- df_sal %>% filter(!is.na(Salary)) %>%
 summarise(mean_salary = mean(Salary)) %>%
 pull(mean_salary)
cat("The mean salary after removing rows with missing values is:", mean_salary)
Using Dataset: Student.csv
df <- read.csv("student.csv")
1. Basic Interaction with R: Compute summary statistics for columns like Math_Score or Science_Score.
summary(df$Math_Score)
summary(df$Science_Score)
2. Indexing Vectors: Extract specific rows or columns using indexing, e.g.,
student_data$Age[2].
df$Age[2]
3. Vectorized Expressions: Perform column-wise operations, such as adding Math_Score and
Science_Score.
df$Total_Score <- df$Math_Score + df$Science_Score
df$Average_Score <- rowMeans(df[, c("Math_Score", "Science_Score")], na.rm = TRUE)
4. Comments: Write clear comments to document each step of data manipulation.
5. Functions: Use built-in and custom functions to calculate averages or categorize grades.
assign_grade <- function(score) {
 if (score >= 85) {
 return("A")
 } else if (score >= 70) {
 return("B")
 } else if (score >= 50) {
 return("C")
 } else {
 return("D")
 }
}
df$New_Grade <- sapply(df$Average_Score, assign_grade)
6. Control Structures: Use if statements to check for missing values and loops for iterating through rows.
for (i in 1:nrow(df)) {
 if (is.na(df$Math_Score[i])) {
 cat("Math score is missing for", df$Name[i], "\n")
 }
}
7. Factors: Analyze the Gender or Grade column as factor variables.
df$Gender <- factor(df$Gender)
df$Grade <- factor(df$Grade)
levels(df$Gender)
levels(df$Grade)
8. Data Frames: Add new columns (e.g., BMI) and filter rows based on conditions.
df$BMI <- df$Weight..kg. / (df$Height..cm. / 100)^2
filtered_data <- subset(df, BMI > 10)
9. Dealing with Missing Values: Handle NA values in Math_Score or Weight_kg.
df$Math_Score[is.na(df$Math_Score)] <- mean(df$Math_Score, na.rm = TRUE)
df$Weight_kg[is.na(df$Weight..kg.)] <- median(df$Weight..kg, na.rm = TRUE)
10. Using R Packages: Use dplyr for filtering, summarizing, and arranging the dataset.
filtered_summary <- df %>%
 filter(Age >= 16) %>%
 summarise(mean_math_score = mean(Math_Score, na.rm = TRUE))
print(filtered_summary)
11. Data Pipelines: Write pipelines to clean data, transform scores, and compute final
final_data <- df %>%
 mutate(
 Total_Score = Math_Score + Science_Score,
 Average_Score = rowMeans(select(., Math_Score, Science_Score), na.rm = TRUE)
 ) %>%
 filter(!is.na(Average_Score))
