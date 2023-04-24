## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
install.packages("tidyverse")
library(tidyverse)

# Import the data set.
sales_data <- read_csv("turtle_sales.csv")

# Print the data frame.
print(sales_data)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
filtered_data <- select(sales_data, -c(Ranking, Year, Genre, Publisher))

# View the data frame.
print(filtered_data)

# Check for missing values
missing_values <- filtered_data %>%
  summarise_all(~sum(is.na(.)))
print(missing_values)

# View the descriptive statistics.
summary(filtered_data)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
ggplot(filtered_data, aes(x = NA_Sales, y = EU_Sales)) + geom_point() +
  labs(title = "Scatterplot: NA_Sales vs EU_Sales")

## 2b) Histograms
# Create histograms.
ggplot(filtered_data, aes(x = NA_Sales)) + geom_histogram() +
  labs(title = "Histogram: NA_Sales")
ggplot(filtered_data, aes(x = EU_Sales)) + geom_histogram() +
  labs(title = "Histogram: EU_Sales")

## 2c) Boxplots
# Create boxplots.
ggplot(filtered_data, aes(x = "NA_Sales", y = NA_Sales)) + geom_boxplot() +
  labs(title = "Boxplot: NA_Sales")
ggplot(filtered_data, aes(x = "EU_Sales", y = EU_Sales)) + geom_boxplot() +
  labs(title = "Boxplot: EU_Sales")


###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
# - NA_Sales has a higher mean and higher max value than EU_Sales - this suggests
#   sales are generally higher in NA region.
# - Scatterplot for NA_Sales vs EU_Sales suggests there is a positive correlation
#   between the variables with few outliers in the 
# - Both the histograms for NA_Sales and EU_Sales share a similar distribution of data
#   with the plots showing a peak at the extreme left end showing the data is skewed
#   to the left. This suggests there are more instances of games with lower 
#   sales. 
# - NA_sales has slightly more instances of games with high sales.
# - Based on the boxplots for NA_Sales and EU_Sales, it is observed that NA_Sales 
#   has a higher median number of sales.
# - Both NA_Sales and EU_Sales have outliers in the higher region of values, with
#   NA_Sales having more outliers that are further away from the whiskers of the 
#   boxplot.
###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
print(filtered_data)

# Check output: Determine the min, max, and mean values.
print(sapply(filtered_data[, c("NA_Sales", "EU_Sales", "Global_Sales")], min))
print(sapply(filtered_data[, c("NA_Sales", "EU_Sales", "Global_Sales")], max))
print(sapply(filtered_data[, c("NA_Sales", "EU_Sales", "Global_Sales")], mean))

# View the descriptive statistics.
summary(filtered_data)

###############################################################################

# 2. Determine the impact on sales per product.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sales_by_product <- filtered_data %>%
  group_by(Product) %>%
  summarise(NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales),
            Global_Sales = sum(Global_Sales))

# View the data frame.
print(sales_by_product)

# Explore the data frame.
summary(sales_by_product)


## 2b) Determine which plot is the best to compare game sales.
# Scatterplot with trend line
ggplot(sales_by_product, aes(x = NA_Sales, y = EU_Sales)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", color = "blue") +
  labs(title = "NA_Sales vs EU_Sales")

# Create histograms.
# Histogram with density curve
ggplot(sales_by_product, aes(x = NA_Sales)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve: NA_Sales")

ggplot(sales_by_product, aes(x = EU_Sales)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  labs(title = "Histogram with Density Curve: EU_Sales")

# Create boxplots.
# Boxplot with labeled outliers
ggplot(sales_by_product, aes(x = "NA_Sales", y = NA_Sales)) +
  geom_boxplot(outlier.color = "red", outlier.size = 4) +
  geom_text(aes(label = ifelse(NA_Sales > boxplot.stats(NA_Sales)$stats[5],
                               as.character(Product), "")), vjust = -0.5) +
  labs(title = "Boxplot with Labeled Outliers: NA_Sales")

ggplot(sales_by_product, aes(x = "EU_Sales", y = EU_Sales)) +
  geom_boxplot(outlier.color = "red", outlier.size = 4) +
  geom_text(aes(label = ifelse(EU_Sales > boxplot.stats(EU_Sales)$stats[5],
                               as.character(Product), "")), vjust = -0.5) +
  labs(title = "Boxplot with Labeled Outliers: EU_Sales")

###############################################################################
install.packages("moments")
library(moments)


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Q-Q Plot for NA_Sales
qqnorm(sales_by_product$NA_Sales)
qqline(sales_by_product$NA_Sales)

# Q-Q Plot for EU_Sales
qqnorm(sales_by_product$EU_Sales)
qqline(sales_by_product$EU_Sales)


# Q-Q Plot for Global_Sales
qqnorm(sales_by_product$Global_Sales)
qqline(sales_by_product$Global_Sales)


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages("moments")
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales_by_product$NA_Sales)
shapiro.test(sales_by_product$EU_Sales)
shapiro.test(sales_by_product$Global_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_by_product$NA_Sales)
skewness(sales_by_product$EU_Sales)
skewness(sales_by_product$Global_Sales)
kurtosis(sales_by_product$NA_Sales)
kurtosis(sales_by_product$EU_Sales)
kurtosis(sales_by_product$Global_Sales)

## 3d) Determine correlation
# Determine correlation.
correlation_matrix <- cor(sales_by_product[, c("NA_Sales", "EU_Sales", "Global_Sales")])
print(correlation_matrix)

###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# Scatterplot with trend line
ggplot(sales_by_product, aes(x = NA_Sales, y = EU_Sales)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot with Trend Line: NA_Sales vs EU_Sales")

# Scatterplot with trend line for NA_Sales vs Global_Sales
ggplot(sales_by_product, aes(x = NA_Sales, y = Global_Sales)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot with Trend Line: NA_Sales vs Global_Sales")

# Scatterplot with trend line for EU_Sales vs Global_Sales
ggplot(sales_by_product, aes(x = EU_Sales, y = Global_Sales)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot with Trend Line: EU_Sales vs Global_Sales")

# Create a heatmap to visualize the correlations
heatmap(correlation_matrix, symm = TRUE, col = colorRampPalette(c("red", "white", "blue"))(25))
                             
                      
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
#-	The max value for NA_Sales is approximately 40% higher than the max value of EU_Sales. Interestingly, these max sales values for NA
#   and EU were achieved by the same product, which was product 107.  
#-	For the top 10 products with the highest Global_Sales, the sales from the NA region is consistently a larger contributor than EU_Sales.
#   For certain games like products 254 and 326, a very large majority of its global sales are from the NA region. 
#-	The trend line on the scatterplot implies that products with higher NA_Sales are likely to have a higher EU_Sales as well.
#   The histogram with the density curve shows that there is a larger spread for NA_sales and hence, more variability in that data set.
#   Interestingly, both NA_Sales and EU_Sales have a second smaller peak. The second peak occurs around a value of 12 for NA_Sales and 7.5 
#   for EU_Sales, which implies a presence of a subgroup or cluster within this data set.
#-	The Q-Q plots for NA_Sales, EU_Sales and Global_Sales all follow a similar pattern where a large number of data points closely follow
#   the reference line in the middle section of the plot. However, as on the far right and left of the plot the data points deviate from 
#   the reference line which implies the data might not be normally distributed. 
#-	The Shapiro-Wilk test yielded p-values smaller than 0.05 across NA_Sales, EU_Sales and Global_Sales which suggests the data is not 
#   normally distributed.
#-	A positive skewness score for all 3 sales data indicates the data has a longer tail on the right side. Similarly, the positive 
#   kurtosis scores for the data also indicates a leptokurtic distribution with heavier tails and a more peaked center than a normal distribution.
#-	According to the correlation matrix (heatmap), there are positive correlations between all pairs of sales data columns except between NA_Sales 
#   and EU_Sales, with the strongest correlation between NA_Sales and Global_Sales, followed by the correlation between EU_Sales and Global_Sales. This 
#   suggests that sales in these regions tend to move together, and when sales increase in one region, they are likely to increase 
#   in other regions as well.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
sales_by_product

# Determine a summary of the data frame.
summary(sales_by_product)


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
simple_lm_na <- lm(Global_Sales ~ NA_Sales, data = sales_by_product)
simple_lm_eu <- lm(Global_Sales ~ EU_Sales, data = sales_by_product)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
# Scatter plot for NA_Sales
plot(sales_by_product$NA_Sales, sales_by_product$Global_Sales,
     xlab = "NA_Sales",
     ylab = "Global_Sales",
     main = "NA_Sales vs Global_Sales")
abline(simple_lm_na, col = "red")

# Scatter plot for EU_Sales
plot(sales_by_product$EU_Sales, sales_by_product$Global_Sales,
     xlab = "EU_Sales",
     ylab = "Global_Sales",
     main = "EU_Sales vs Global_Sales")
abline(simple_lm_eu, col = "red")
###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
numeric_data <- sales_by_product[, c("NA_Sales", "EU_Sales", "Global_Sales")]

# Multiple linear regression model.
multiple_lm <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = numeric_data)

# View the summary of the multiple linear regression model
summary(multiple_lm)

# Obtain the coefficients of the multiple linear regression model
coef(multiple_lm)

# Obtain the confidence intervals for the coefficients
confint(multiple_lm)

# Obtain residuals statistics
residuals_stats <- summary(multiple_lm)$sigma
print(residuals_stats)

# Obtain R-squared value
r_squared <- summary(multiple_lm)$r.squared
print(r_squared)

# Obtain adjusted R-squared value
adj_r_squared <- summary(multiple_lm)$adj.r.squared
print(adj_r_squared)

###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.
values <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                     EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))


predicted_global_sales <- predict(multiple_lm, values)

# Print predicted values
print(predicted_global_sales)

# Actual Global_Sales values
Global_Sales_observed <- c(67.85, 6.04, 4.32, 3.53, 23.21)

# Calculate the percentage difference between observed and predicted Global_Sales
percentage_difference <- (predicted_global_sales - Global_Sales_observed) / Global_Sales_observed * 100

# Create a data frame with the actual, predicted Global_Sales values and percentage difference
comparison <- data.frame(Actual_Global_Sales = Global_Sales_observed, 
                         Predicted_Global_Sales = predicted_global_sales,
                         Percentage_Difference = percentage_difference)

# Print the comparison
print(comparison)

###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# - NA_Sales and EU_Sales both have a positive coefficient indicating an increase 
#  in these variables result in an increase in Global_Sales.
# - The model has a very high R-squared value of 0.9668  suggesting it has a very 
#   strong explanatory power 
# - Overall, the multiple linear regression model demonstrates a strong relationship
#   between NA_Sales, EU_Sales, and Global_Sales. The model has a high R-squared value,
#   indicating that a significant portion of the variation in Global_Sales can be explained
#   by NA_Sales and EU_Sales
# - Overall, the multiple linear regression model seems to perform well in most cases,
#   with only one relatively weak prediction. This suggests that the model is generally
#   strong and provides useful predictions for Global_Sales.
# - Suggestions to improve model include considering additional variables that may
#   influence global sales such as marketing efforts or product release dates.
# - The strong relationships between regional sales and global sales indicate that
#   Turtle Games should prioritize increasing sales in both North America and Europe to maximize 
#   global sales. This could involve tailoring marketing efforts to each region's preferences,
#   understanding regional differences in gaming culture, or developing products that cater 
#   specifically to these markets.


###############################################################################
###############################################################################




