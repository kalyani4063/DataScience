install.packages("dplyr")
library(dplyr)
data<-mtcars
# Separate numerical and categorical variables
numerical_vars <- names(data)[sapply(data, is.numeric)]
categorical_vars <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
print("Numerical Variables:")
print(numerical_vars)
print("Categorical Variables:")
print(categorical_vars)
install.packages("esquisse")
install.packages("shiny")
install.packages("ggplot2")
library(ggplot2)
#2nd question
ggplot(data) +
 aes(x = factor(cyl), y = mpg) +
 geom_point(colour = "#112446") +
 geom_boxplot(aes(x = factor(cyl), y = mpg), fill = "#C9155E") +
 theme_minimal()+
 ggtitle("MPG by Cylinder Count")
#3rd question jitter points to boxplot
ggplot(data) +
 aes(x = factor(cyl), y = mpg) +
 geom_point(colour = "#112446") +
 geom_boxplot(aes(x = factor(cyl), y = mpg), fill = "#C9155E") +
 geom_jitter(color = "orange", width = 0.2, alpha = 0.5) +
 theme_minimal()
#4th qn
ggplot(data) +
 aes(x = disp) +
 geom_histogram(bins = 30L, fill = "#DC9F67") +
 labs(title = "HISTOGRAM FOR DISP") +
 theme_minimal() +
 theme(
 plot.title = element_text(face = "italic",
 hjust = 0.5)
 )
#5th qn
ggplot(data) +
 aes(x = drat) +
geom_histogram(bins = 30L, fill = "#91C894") +
 geom_density(color = "red") +
 theme_minimal()
#6th qn scatter plot
ggplot(data) +
 aes(x = hp, y = qsec) +
 geom_point(size = 3.1, colour = "#10A1AD") +
 labs(title = "relationship between hp and q sec") +
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5))
#7TH QN trend line in scatter plot
ggplot(data) +
 aes(x = hp, y = qsec) +
 geom_point(size = 2.1, colour = "#10A1AD") +
 geom_smooth(method = "lm", color = "red") +
 labs(title = "relationship between hp and q sec") +
 theme_minimal() +
 theme(plot.title = element_text(hjust = 0.5))
#8th qn
ggplot(data, aes(x = mpg)) +
 geom_boxplot(fill="red") +
 facet_wrap(~ factor(cyl)) +
 ggtitle("Box Plots by Categorical Column")
#9TH QN
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
 geom_violin() +
 labs(title = "Violin Plot of MPG by Cylinder",
 x = "Number of Cylinders",
 y = "Miles Per Gallon (MPG)") +
 theme_minimal()
#10th qn
ggplot(data, aes(x = factor(cyl))) +
 geom_bar(fill = "blue", color = "black") +
 ggtitle("Bar Plot of Categorical Column Counts")
#11th qn
library(reshape2)
# Compute correlation matrix
cor_matrix <- cor(data[sapply(data, is.numeric)], use = "complete.obs")
melted_cor <- melt(cor_matrix)
# Create heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
 geom_tile() +
 scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
 ggtitle("Heatmap of Correlations") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))
2. iris dataset
#IRIS DATASET
install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)
data(iris)
data(mtcars)
#Data Set : Iris Dataset
#1. Create a Shiny app using the iris dataset. The app should allow users to select a
#numerical variable (e.g., Sepal.Length, Petal.Width) and a categorical variable (e.g.,
#Species). Once the variables are selected, the app should display summary statistics
#(mean, median, standard deviation, etc.) for the chosen numerical variable, grouped by
#the selected categorical variable.
ui1 <- fluidPage(
 titlePanel("Summary Statistics - Iris Dataset"),
 sidebarLayout(
 sidebarPanel(``
 selectInput("num_var", "Choose a numerical variable:", choices = names(iris)[1:4]),
 selectInput("cat_var", "Choose a categorical variable:", choices = names(iris)[5])
 ),
 mainPanel(
 tableOutput("summary_table")
 )
 )
)
server1 <- function(input, output) {
 output$summary_table <- renderTable({
 req(input$num_var, input$cat_var)
 iris %>%
 group_by(.data[[input$cat_var]]) %>%
 summarize(
 Mean = mean(.data[[input$num_var]]),
 Median = median(.data[[input$num_var]]),
 SD = sd(.data[[input$num_var]]),
 Min = min(.data[[input$num_var]]),
 Max = max(.data[[input$num_var]])
 )
 }, rownames = TRUE)
}
shinyApp(ui1, server1)
2
ND QN
#2. Build a Shiny app where users can select two numerical variables (e.g., Sepal.Length,
#Petal.Width) for the x and y axes, and a categorical variable (e.g., Species) to color the
#points in a scatter plot.
ui2 <- fluidPage(
 titlePanel("Scatter Plot - Iris Dataset"),
 sidebarLayout(
 sidebarPanel(
 selectInput("x_var", "Select X-axis variable:", choices = names(iris)[1:4]),
 selectInput("y_var", "Select Y-axis variable:", choices = names(iris)[1:4]),
 selectInput("cat_var", "Select color grouping (categorical):", choices = names(iris)[5])
 ),
 mainPanel(
 plotOutput("scatter_plot")
 )
 )
)
server2 <- function(input, output) {
 output$scatter_plot <- renderPlot({
 req(input$x_var, input$y_var, input$cat_var)
 ggplot(iris, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], color = .data[[input$cat_var]])) +
 geom_point(size = 3, alpha = 0.7) +
 theme_minimal() +
 labs(
 x = input$x_var,
 y = input$y_var,
 color = input$cat_var,
 title = paste("Scatter Plot:", input$x_var, "vs", input$y_var)
 )
 })
}
shinyApp(ui2, server2)
3
RD QN
#IRIS DATASET
install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)
data(iris)
data(mtcars)
#3. Create a Shiny app that allows users to choose a numerical variable (e.g., Sepal.Length,
#Petal.Width) and a categorical variable (e.g., Species) to generate a box plot.
# Load libraries
library(shiny)
library(ggplot2)
ui3 <- fluidPage(
 titlePanel("Box Plot - Iris Dataset"),
 sidebarLayout(
 sidebarPanel(
 selectInput("num_var", "Select a numerical variable:", choices = names(iris)[1:4]),
 selectInput("cat_var", "Select a categorical variable:", choices = names(iris)[5])
 ),
 mainPanel(
 plotOutput("box_plot")
 )
 )
)
server3 <- function(input, output) {
 output$box_plot <- renderPlot({
 req(input$num_var, input$cat_var)
 ggplot(iris, aes(x = .data[[input$cat_var]], y = .data[[input$num_var]], fill = .data[[input$cat_var]]))
+
 geom_boxplot(alpha = 0.7) +
 theme_minimal() +
 labs(
 x = input$cat_var,
 y = input$num_var,
 title = paste("Box Plot of", input$num_var, "by", input$cat_var)
 ) +
 scale_fill_brewer(palette = "Set3")
 })
}
shinyApp(ui3 , server3)
