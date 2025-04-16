# Load necessary libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
library(ggplot2)
library(dplyr)
library(readr)
# Load dataset
sales_data <- read.csv("sales_data.csv") # Replace with actual file path
# Check for missing values
print("Missing values in each column:")
print(colSums(is.na(sales_data)))
# Handle missing values (fill numerical columns with median)
sales_data$Price[is.na(sales_data$Price)] <- median(sales_data$Price, na.rm = TRUE)
sales_data$Cost[is.na(sales_data$Cost)] <- median(sales_data$Cost, na.rm = TRUE)
sales_data$Quantity[is.na(sales_data$Quantity)] <- median(sales_data$Quantity, na.rm = TRUE)
# Convert categorical variables into factors
sales_data$Product <- as.factor(sales_data$Product)
sales_data$Category <- as.factor(sales_data$Category)
sales_data$Region <- as.factor(sales_data$Region)
# Compute summary statistics for Revenue and Profit
print("Summary Statistics for Revenue and Profit:")
print(summary(sales_data$Revenue))
print(summary(sales_data$Profit))
#right skewed , Mean > Median > Mode
# Boxplot to visualize revenue distribution
# Simple Boxplot for Revenue
boxplot(sales_data$Revenue,
 main = "Boxplot of Revenue",
 ylab = "Revenue",
 col = "lightblue",
 border = "darkblue")
# Mode function
mode_function <- function(x) {
 uniq_x <- unique(x)
 uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
print(paste("Mode of Revenue:", mode_function(sales_data$Revenue)))
print(paste("Mode of Profit:", mode_function(sales_data$Profit)))
# Top 5 selling products
top_products <- sales_data %>%
 group_by(Product) %>%
 summarise(Total_Sales = sum(Quantity)) %>%
 arrange(desc(Total_Sales)) %>%
 head(top_products)
ggplot(top_products, aes(x = reorder(Product, -Total_Sales), y = Total_Sales, fill = Product)) +
 geom_bar(stat = "identity") +
 labs(title = "Top 5 Selling Products", x = "Product", y = "Total Sales") +
 theme_minimal()
# Seasonal sales trends (time-series plot)
# Convert Date column to Date type
sales_data$Date <- as.Date(sales_data$Date, format="%Y-%m-%d")
# Aggregate monthly sales
monthly_sales <- sales_data %>%
 group_by(Month = as.Date(paste0(format(Date, "%Y-%m"), "-01"))) %>% # Convert to first day of
the month
 summarise(Total_Revenue = sum(Revenue))
# Plot time series
ggplot(monthly_sales, aes(x = Month, y = Total_Revenue)) + # Month is now a Date type
 geom_line(color = "blue") +
 labs(title = "Monthly Sales Trend", x = "Month", y = "Total Revenue") +
 theme_minimal()
# Products with high sales but low profit margins
profitability <- sales_data %>%
 group_by(Product) %>%
 summarise(Total_Sales = sum(Quantity), Total_Profit = sum(Profit)) %>%
 mutate(Profit_Margin = Total_Profit / Total_Sales) %>%
 arrange(Profit_Margin)
print("Products with lowest profit margins:")
print(head(profitability))
# Profit contribution per region
region_profit <- sales_data %>%
 group_by(Region) %>%
 summarise(Total_Profit = sum(Profit))
ggplot(region_profit, aes(x = reorder(Region, -Total_Profit), y = Total_Profit, fill = Region)) +
 geom_bar(stat = "identity") +
 labs(title = "Profit Contribution per Region", x = "Region", y = "Total Profit") +
 theme_minimal()
# Price elasticity of demand (PED) function
calculate_ped <- function(price1, price2, quantity1, quantity2) {
 percentage_change_quantity <- ((quantity2 - quantity1) / quantity1) * 100
 percentage_change_price <- ((price2 - price1) / price1) * 100
 ped <- percentage_change_quantity / percentage_change_price
 return(ped)
}
#%change in quantity demanded/#%change in price
#Price Elasticity of Demand -Price Elasticity of Demand (PED) measures
#how the quantity demanded of
#a product changes in response to a change in its price.
# Example PED calculation (modify values as needed)
ped_value <- calculate_ped(100, 90, 500, 550)
print(paste("Price Elasticity of Demand:", ped_value))
#If |PED| > 1, demand is elastic.
#If |PED| < 1, demand is inelastic.
#If |PED| = 1, demand is unit elastic.
# Classify products as elastic or inelastic
sales_data <- sales_data %>%
 mutate(Elasticity = ifelse(Revenue / Quantity < -1, "Elastic", "Inelastic"))
print("Count of Elastic and Inelastic products:")
print(table(sales_data$Elasticity))
#Inelastic demand means that a change in price has little effect on quantity demanded. eg. medicine
# Recommend price adjustments based on elasticity
recommendation <- sales_data %>%
 group_by(Product) %>%
 summarise(Average_Elasticity = mean(Revenue / Quantity)) %>%
 mutate(Price_Recommendation = ifelse(Average_Elasticity < -1, "Reduce Price", "Increase Price"))
print("Price Adjustment Recommendations:")
print(recommendation)
# Suggest discount strategies
discount_strategy <- sales_data %>%
 group_by(Product) %>%
 summarise(Average_Elasticity = mean(Revenue / Quantity)) %>%
 mutate(Discount_Strategy = ifelse(Average_Elasticity < -1, "Offer Discounts", "Minimal Discounts"))
print("Discount Strategy Recommendations:")
print(discount_strategy)
# Recommend optimal inventory levels
inventory_levels <- sales_data %>%
 group_by(Product) %>%
 summarise(Average_Sales = mean(Quantity)) %>%
 mutate(Stock_Recommendation = ifelse(Average_Sales > 50, "High Stock", "Low Stock"))
print("Optimal Inventory Levels:")
print(inventory_levels)
# Regional sales strategies
regional_strategy <- sales_data %>%
 group_by(Region) %>%
 summarise(Total_Sales = sum(Quantity)) %>%
 mutate(Sales_Strategy = ifelse(Total_Sales > mean(Total_Sales), "Expand Marketing", "Targeted
Promotions"))
print("Regional Sales Strategies:")
print(regional_strategy)
