#######################Data Wrangling###############################
# load required packages
library(ggplot2)
library(dplyr)
library(jsonlite)
library(tidyverse)
library(caret)
library(cluster)
# specify file path
file_path <- "D:/sales.json"
# read json file into R
sales <- stream_in(file(file_path))

# Convert JSON file to a data frame.
sales_data <- as.data.frame(sales)

str(sales_data)
# drop columns from sales_data
sales_data <- sales_data[,-c(1,2,3)]
# view the resulting data frame
sales_data
############# Data Transformation########################

# export as CSV file
write.csv(sales_data, "D:/sales_data.csv", row.names = FALSE)
######################## Data Analysis##################
# read in the data
sales_data <- read.csv("D:/sales_data.csv")


# Explore the data
head(sales_data)
summary(sales_data)

# Statistical Analysis
# Mean and standard deviation age of customers
mean_age <- mean(sales_data$customer.age)
cat("The mean age of customers is:", mean_age, "\n")
sd_age <- sd(sales_data$customer.age)
cat("The standard deviation of customer age is:", sd_age, "\n")

# Proportion of male and female customers
gender_prop <- sales_data %>%
  group_by(customer.gender) %>%
  summarize(count = n()) %>%
  mutate(prop = count/sum(count) * 100)
cat("The proportion of male and female customers is:", gender_prop$prop, "\n")

# Proportion of purchases made with a coupon
coupon_prop <- sales_data %>%
  group_by(couponUsed) %>%
  summarize(count = n()) %>%
  mutate(prop = count/sum(count) * 100)
cat("The proportion of purchases made with a coupon is:", coupon_prop$prop[2], "\n")

# Visualizations
# Histogram of customer age distribution
ggplot(sales_data, aes(x = customer.age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  ggtitle("Customer Age Distribution")

# Bar chart of customer satisfaction levels
ggplot(sales_data, aes(x = factor(customer.satisfaction), fill = factor(customer.satisfaction))) +
  geom_bar() +
  scale_fill_manual(values = c("#e74c3c", "#f1c40f", "#2ecc71", "#3498db", "#9b59b6")) +
  ggtitle("Customer Satisfaction Levels")

# Pie chart of purchase methods
ggplot(sales_data, aes(x = "", fill = purchaseMethod)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  ggtitle("Purchase Methods")

# Stacked bar chart of purchases by location and gender
ggplot(sales_data, aes(x = storeLocation, fill = customer.gender)) +
  geom_bar() +
  ggtitle("Purchases by Location and Gender")
#############Data Modelling#########################

# Prepare the data
# Convert categorical variables to factors
sales_data$storeLocation <- as.factor(sales_data$storeLocation)
sales_data$customer.gender <- as.factor(sales_data$customer.gender)
sales_data$couponUsed <- as.factor(sales_data$couponUsed)
sales_data$purchaseMethod <- as.factor(sales_data$purchaseMethod)

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(sales_data$customer.satisfaction, p = 0.7, list = FALSE)
train_data <- sales_data[train_indices, ]
test_data <- sales_data[-train_indices, ]
train_data$customer.satisfaction <- factor(train_data$customer.satisfaction, levels = 1:5)
test_data$customer.satisfaction <- factor(test_data$customer.satisfaction, levels = 1:5)

#Supervisesd data Modelling 
suppressMessages(library(randomForest))

# Training with Random forest model
modfit.rf <- randomForest(customer.satisfaction ~. , data=train_data)

# Predict the testing set with the trained model
predictions2 <- predict(modfit.rf, test_data, type = "class")
levels(predictions2)
levels(test_data$customer.satisfaction)

# Convert the predictions to a factor with the same levels as test_data$customer.satisfaction
predictions2 <- factor(predictions2, levels = levels(factor(test_data$customer.satisfaction)))

# Calculate the confusion matrix
confusionMatrix(predictions2, test_data$customer.satisfaction)

##### Unsupervised Model
# select the columns for clustering
data <- sales_data[, c("customer.age", "customer.satisfaction")]

# scale the data
data_scaled <- scale(data)

# set the number of clusters to create
k <- 3
# perform k-means clustering
kmeans_fit <- kmeans(data_scaled, k)
# get the cluster centers
centers <- kmeans_fit$centers
# get the cluster assignments for each data point
cluster_assignments <- kmeans_fit$cluster
# plot the clusters
plot(data_scaled, col = cluster_assignments, pch = 20, main = "k-means clustering")
points(centers, col = 1:k, pch = 4, cex = 2)
