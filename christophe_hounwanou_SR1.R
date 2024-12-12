#useful librairies
library(readr) # read csv
library(dplyr) # data manipulation
library(caret) 
library(class) # model
library(ggplot2) # plots
library(corrplot) # Correlation plots



#1) Perform a descriptive analysis of the variables in this dataset.


#import the dataset

data <- read.csv("C:/Users/HP/Desktop/AIMS/Review phase/S.Regression/New folder/data_purchase_behaviour.csv")
View(data)

# Summary of variables
summary(data)

# Frequency of categorical variables
table(data$Gender)
table(data$City_Category)

# Visualize age distribution
hist(data$Age_num, 
     main = "Age distribution of customers", 
     xlab = "Age", 
     col = "skyblue", 
     border = "black")

# Boxplot of purchase amount by gender
boxplot(data$Purchase ~ data$Gender, 
        main = "Purchase Amount by Gender", 
        xlab = "Gender", 
        ylab = "Purchase Amount", 
        col = c("pink", "lightblue"), 
        border = "black")

# Barplot for City Category
barplot(table(data$City_Category), 
        main = "Number of customers by city category", 
        xlab = "City Category", 
        ylab = "Number of Customers", 
        col = c("black", "skyblue", "yellow"), 
        border = "black")

####################################

# 2-a) Fit the simple linear regression model
model <- lm(Purchase ~ Age_num, data = data)
summary(model)  # Display model coefficients, R-squared, and p-values

# Extract model coefficients and uncertainties
intercept <- coef(model)[1]
slope <- coef(model)[2]
confint(model)  # Confidence intervals for the coefficients

# 2b) Visualize the regression model
ggplot(data, aes(x = Age_num, y = Purchase)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Simple Linear Regression: Purchase amount vs. Age",
    x = "Age",
    y = "Purchase amount"
  ) +
  theme_minimal()

# Calculate means
mean_x <- mean(data$Age_num)  # Mean of Age
mean_y <- mean(data$Purchase) # Mean of Purchase

# Calculate numerator: covariance
numerator <- sum((data$Age_num - mean_x) * (data$Purchase - mean_y))

# Calculate denominator: product of standard deviations
denominator <- sqrt(sum((data$Age_num - mean_x)^2) * sum((data$Purchase - mean_y)^2))

# Correlation coefficient
correlation_coefficient <- numerator / denominator
correlation_coefficient


############################################"

# 3-  Investigate association between Purchase and Gender
  
  # Summary statistics by gender
  aggregate(Purchase ~ Gender, data = data, summary)
# Boxplot to visualize distribution of purchase amounts by gender
boxplot(data$Purchase ~ data$Gender,
        main = "Purchase Amount by Gender",
        xlab = "Gender",
        ylab = "Purchase Amount",
        col = c("pink", "lightblue"))

# Perform t-test to compare purchase amounts between genders
t_test_result <- t.test(Purchase ~ Gender, data = data)
t_test_result

