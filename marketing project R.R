# Load necessary libraries
library(tidyverse)
library(fastDummies)
library(corrplot)
library(randomForest)
library(caret)

# Set working directory and load data
setwd("C:/Users/Asus/Downloads")
shopping_data <- read.csv("archive.csv")

# View the first few rows of the dataset
head(shopping_data)

# Handle missing values by removing rows with any NA values
shopping_data <- shopping_data %>%
  drop_na()

# Convert relevant columns to factors for categorical variables
shopping_data <- shopping_data %>%
  mutate(across(c(Gender, Category, Location, Season, Subscription.Status, 
                  Payment.Method, Shipping.Type, Item.Purchased, Size, Color, 
                  Discount.Applied, Promo.Code.Used, Previous.Purchases, 
                  Preferred.Payment.Method, Frequency.of.Purchases), 
                as.factor))

# Group 'Location' into 'Region' for easier analysis
shopping_data <- shopping_data %>%
  mutate(
    Region = case_when(
      Location %in% c("Alabama", "Florida", "Georgia", "Mississippi", "South Carolina", "Louisiana", "Arkansas", "Tennessee", "Kentucky") ~ "South",
      Location %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New York", "New Jersey", "Pennsylvania") ~ "Northeast",
      Location %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Minnesota", "Iowa", "Missouri", "Kansas", "Nebraska", "South Dakota", "North Dakota") ~ "Midwest",
      Location %in% c("California", "Nevada", "Oregon", "Washington", "Arizona", "New Mexico", "Colorado", "Utah", "Alaska", "Hawaii", "Idaho", "Montana", "Wyoming") ~ "West",
      Location %in% c("Texas", "Oklahoma") ~ "Southwest",
      TRUE ~ "Other"
    )
  )

# Convert 'Region' to a factor
shopping_data$Region <- as.factor(shopping_data$Region)

# Create binary variables for Gender, Subscription Status, Discount, and Promo Code Usage
shopping_data <- shopping_data %>%
  mutate(
    Gender = ifelse(Gender == "Male", 1, 0), 
    Subscription.Status = ifelse(Subscription.Status == "Yes", 1, 0),
    Discount.Applied = ifelse(Discount.Applied == "Yes", 1, 0),
    Promo.Code.Used = ifelse(Promo.Code.Used == "Yes", 1, 0)
  )

# Check for the first few rows after transformations
head(shopping_data)

# Check final structure of the dataset
str(shopping_data)

# Summarize the dataset to understand distribution of variables
summary(shopping_data)

# Check for missing values in the dataset
sum(is.na(shopping_data))

# Visualizing the distribution of Purchase Amount (histogram)
ggplot(shopping_data, aes(x = Purchase.Amount..USD.)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Purchase Amount", x = "Purchase Amount (USD)", y = "Frequency") +
  theme_minimal()

# Ensure Previous.Purchases is numeric and handle NAs
shopping_data$Previous.Purchases <- as.numeric(as.character(shopping_data$Previous.Purchases))

# Remove rows with NA in Previous.Purchases
shopping_data <- shopping_data %>%
  filter(!is.na(Previous.Purchases))

# Plot the distribution of Previous Purchases (histogram)
ggplot(shopping_data, aes(x = Previous.Purchases)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Previous Purchases", x = "Previous Purchases", y = "Frequency") +
  theme_minimal()

# Summarize average Purchase Amount by Gender
gender_summary <- shopping_data %>%
  group_by(Gender) %>%
  summarise(
    Mean_Purchase = mean(Purchase.Amount..USD., na.rm = TRUE),
    SE = sd(Purchase.Amount..USD., na.rm = TRUE) / sqrt(n())
  )

# Visualize Average Purchase Amount by Gender with error bars
ggplot(gender_summary, aes(x = Gender, y = Mean_Purchase, fill = as.factor(Gender))) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  geom_errorbar(aes(ymin = Mean_Purchase - SE, ymax = Mean_Purchase + SE), width = 0.2) +
  labs(title = "Average Purchase Amount by Gender", x = "Gender", y = "Mean Purchase Amount (USD)") +
  scale_fill_manual(values = c("lightblue", "lightgreen")) +
  theme_minimal()


# T-test for Gender and Purchase Amount
t_test_result <- t.test(Purchase.Amount..USD. ~ Gender, data = shopping_data)
print(t_test_result)

# ANOVA for Region and Purchase Amount
anova_result <- aov(Purchase.Amount..USD. ~ Region, data = shopping_data)
summary(anova_result)

# Fit a linear regression model to predict Purchase Amount
lm_model <- lm(Purchase.Amount..USD. ~ Gender + Age + Previous.Purchases + Region + Discount.Applied + Promo.Code.Used, data = shopping_data)

# Model summary and evaluation
summary(lm_model)

# Residual plots to assess assumptions of the linear regression model
plot(lm_model, 1)

# Q-Q plot for normality of residuals
plot(lm_model, 2)

# Scale-Location plot for homoscedasticity
plot(lm_model, 3)

# Residuals vs Leverage plot for influential points
plot(lm_model, 5)

# Interaction model: Including interaction between Gender and Region
lm_interaction <- lm(Purchase.Amount..USD. ~ Gender * Region + Age + Previous.Purchases + Discount.Applied + Promo.Code.Used, data = shopping_data)
summary(lm_interaction)

# K-means clustering (Segmentation of customers)
kmeans_result <- kmeans(shopping_data %>% select(Purchase.Amount..USD., Previous.Purchases, Age), centers = 4)
shopping_data$Cluster <- kmeans_result$cluster

# Visualize customer clusters based on Purchase Amount and Previous Purchases
ggplot(shopping_data, aes(x = Purchase.Amount..USD., y = Previous.Purchases, color = factor(Cluster))) +
  geom_point() +
  labs(title = "Customer Segments based on Purchase Amount and Previous Purchases", x = "Purchase Amount (USD)", y = "Previous Purchases") +
  theme_minimal()

# Logistic regression model to predict Promo.Code.Used
log_model <- glm(Promo.Code.Used ~ Gender + Region + Previous.Purchases + Age, data = shopping_data, family = "binomial")
summary(log_model)

# Evaluate the logistic regression model
predicted_classes <- ifelse(predict(log_model, type = "response") > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predicted_classes, Actual = shopping_data$Promo.Code.Used)
print(confusion_matrix)

# Random Forest model to predict Purchase Amount (Regression)
rf_model <- randomForest(Purchase.Amount..USD. ~ Gender + Age + Region + Previous.Purchases + Promo.Code.Used, data = shopping_data, ntree = 100)
print(rf_model)

# Plot feature importance from Random Forest model
importance(rf_model)

# Evaluate the Random Forest model performance (MSE)
predicted_rf <- predict(rf_model, newdata = shopping_data)
mse_rf <- mean((shopping_data$Purchase.Amount..USD. - predicted_rf)^2)
print(paste("Random Forest Model MSE:", mse_rf))

# Summarize seasonal purchase trends
shopping_data %>%
  group_by(Season) %>%
  summarise(Seasonal_Purchase = sum(Purchase.Amount..USD., na.rm = TRUE)) %>%
  ggplot(aes(x = Season, y = Seasonal_Purchase, fill = Season)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Seasonal Purchase Trends", x = "Season", y = "Total Purchase Amount") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")  # Optional color palette

# Compare purchase amounts between customers with and without promo codes
t_test_promo <- t.test(Purchase.Amount..USD. ~ Promo.Code.Used, data = shopping_data)
print(t_test_promo)

# For regression models: calculate Mean Squared Error (MSE)
predicted_values <- predict(lm_model, newdata = shopping_data)
mse_lm <- mean((shopping_data$Purchase.Amount..USD. - predicted_values)^2)
print(paste("Linear Regression MSE:", mse_lm))

# Load necessary libraries
library(dplyr)
library(ggplot2)

# 'shopping_data' is the dataset and has the following columns:
# Gender, Region, Age, Previous.Purchases, Subscription.Status,
# Discount.Applied, Promo.Code.Used, Item.Purchased, Season, Payment.Method,
# Shipping.Type, Color, Purchase.Amount..USD.

# Step 1: a linear regression model to predict Purchase.Amount..USD.
lm_model <- lm(Purchase.Amount..USD. ~ Gender + Region + Age + Previous.Purchases + Subscription.Status + 
                 Discount.Applied + Promo.Code.Used + Item.Purchased + Season + Payment.Method + 
                 Shipping.Type + Color, data = shopping_data)

# Step 2: Predict Purchase Amount based on the model
shopping_data$Predicted_Purchase_Amount <- predict(lm_model, newdata = shopping_data)

# Step 3: Create a scoring system based on the predicted purchase amount
# Customers with a predicted purchase amount higher than the median are classified as 'High' consumers
shopping_data$Score <- ifelse(shopping_data$Predicted_Purchase_Amount > median(shopping_data$Predicted_Purchase_Amount), "High", "Low")

# Step 4: Analyze the trends for each feature (Item.Purchased, Season, Payment.Method, Shipping.Type, Color)

# Analyze Item.Purchased trends
item_trends <- shopping_data %>%
  group_by(Item.Purchased) %>%
  summarise(Avg_Predicted_Purchase = mean(Predicted_Purchase_Amount)) %>%
  arrange(desc(Avg_Predicted_Purchase))
print(item_trends)

# Analyze Season trends
seasonal_trends <- shopping_data %>%
  group_by(Season) %>%
  summarise(Avg_Predicted_Purchase = mean(Predicted_Purchase_Amount)) %>%
  arrange(desc(Avg_Predicted_Purchase))
print(seasonal_trends)

# Analyze Payment.Method trends
payment_method_trends <- shopping_data %>%
  group_by(Payment.Method) %>%
  summarise(Avg_Predicted_Purchase = mean(Predicted_Purchase_Amount)) %>%
  arrange(desc(Avg_Predicted_Purchase))
print(payment_method_trends)

# Analyze Shipping.Type trends
shipping_type_trends <- shopping_data %>%
  group_by(Shipping.Type) %>%
  summarise(Avg_Predicted_Purchase = mean(Predicted_Purchase_Amount)) %>%
  arrange(desc(Avg_Predicted_Purchase))
print(shipping_type_trends)

#Analyze Size trends
size_trends <- shopping_data %>%
  group_by(Size) %>%
  summarise(Avg_Predicted_Purchase = mean(Predicted_Purchase_Amount)) %>%
  arrange(desc(Avg_Predicted_Purchase))
print(size_trends)

# Visualize Size trends
ggplot(size_trends, aes(x = reorder(Size, -Avg_Predicted_Purchase), y = Avg_Predicted_Purchase, fill = Size)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(
    title = "Average Predicted Purchase Amount by Size",
    x = "Size",
    y = "Average Predicted Purchase Amount"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Analyze Region trends
region_trends <- shopping_data %>%
  group_by(Region) %>%
  summarise(Avg_Predicted_Purchase = mean(Predicted_Purchase_Amount)) %>%
  arrange(desc(Avg_Predicted_Purchase))
print(region_trends)
# Step 5: Visualizing the predictions and scores

# Plotting predicted purchase amount by Region
ggplot(shopping_data, aes(x = Region, y = Predicted_Purchase_Amount, color = Score)) + 
  geom_boxplot() + 
  labs(title = "Predicted Purchase Amount by Region", x = "Region", y = "Predicted Purchase Amount")

# Plotting predicted purchase amount by Item.Purchased
ggplot(shopping_data, aes(x = Item.Purchased, y = Predicted_Purchase_Amount, color = Score)) + 
  geom_boxplot() + 
  labs(title = "Predicted Purchase Amount by Item.Purchased", x = "Item Purchased", y = "Predicted Purchase Amount")

# Plotting predicted purchase amount by Payment.Method
ggplot(shopping_data, aes(x = Payment.Method, y = Predicted_Purchase_Amount, color = Score)) + 
  geom_boxplot() + 
  labs(title = "Predicted Purchase Amount by Payment Method", x = "Payment Method", y = "Predicted Purchase Amount")

# Plotting predicted purchase amount by Shipping.Type
ggplot(shopping_data, aes(x = Shipping.Type, y = Predicted_Purchase_Amount, color = Score)) + 
  geom_boxplot() + 
  labs(title = "Predicted Purchase Amount by Shipping Type", x = "Shipping Type", y = "Predicted Purchase Amount")
# Plotting predicted purchase amount by Size
ggplot(shopping_data, aes(x = Size, y = Predicted_Purchase_Amount, color = Score)) + 
  geom_boxplot() + 
  labs(title = "Predicted Purchase Amount by Size", x = "Size", y = "Predicted Purchase Amount")



# Step 6: Final Analysis of Top Performing Segments

# Extracting top-performing regions, items, payment methods, etc.
top_performers <- shopping_data %>%
  filter(Score == "High") %>%
  summarise(
    Top_Region = Region[which.max(Predicted_Purchase_Amount)],
    Top_Item = Item.Purchased[which.max(Predicted_Purchase_Amount)],
    Top_Payment_Method = Payment.Method[which.max(Predicted_Purchase_Amount)],
    Top_Shipping_Type = Shipping.Type[which.max(Predicted_Purchase_Amount)],
    Top_Season = Season[which.max(Predicted_Purchase_Amount)],
    Top_Size = Size[which.max(Predicted_Purchase_Amount)]
  )

print(top_performers)


# Step 7: Summary of Analysis
summary(shopping_data)

# Chi-Square Test for Independence
chisq_test <- chisq.test(table(shopping_data$Region, shopping_data$Subscription.Status))
print(chisq_test)

# Interpret the results
if (chisq_test$p.value < 0.05) {
  cat("Region and Subscription Status are significantly associated.\n")
} else {
  cat("No significant association between Region and Subscription Status.\n")
}

# One-way ANOVA for Purchase Amount by Region
anova_model <- aov(Purchase.Amount..USD. ~ Region, data = shopping_data)
summary(anova_model)

# Tukey's HSD for pairwise comparisons
tukey_results <- TukeyHSD(anova_model)
print(tukey_results)

# Visualize pairwise differences
plot(tukey_results)

# Clustering: Elbow Method and Silhouette 
install.packages("factoextra")

library(factoextra)

# Elbow Method
fviz_nbclust(shopping_data[, c("Purchase.Amount..USD.", "Age")], kmeans, method = "wss") +
  ggtitle("Elbow Method")

# Silhouette Method
fviz_nbclust(shopping_data[, c("Purchase.Amount..USD.", "Age")], kmeans, method = "silhouette") +
  ggtitle("Silhouette Method")


# Breusch-Pagan Test for Heteroscedasticity
library(lmtest)
bptest(lm_mode)
# If p-value < 0.05, heteroscedasticity is present.

# Clustering Enhancements with PCA
# Principal Component Analysis
pca_results <- prcomp(shopping_data[, c("Purchase.Amount..USD.", "Age", "Previous.Purchases")], scale = TRUE)
summary(pca_results)

# Visualize PCA
fviz_pca_biplot(pca_results, geom = "point", habillage = shopping_data$Region, addEllipses = TRUE)

# Advanced Statistical Testing: Cross-Validation
library(caret)

# Define training control
train_control <- trainControl(method = "cv", number = 10)

# Train linear model with cross-validation
cv_model <- train(Purchase.Amount..USD. ~ Age + Gender + Previous.Purchases,
                  data = shopping_data, 
                  method = "lm",
                  trControl = train_control)

# Summary of cross-validated model
print(cv_model)

# Descriptive Statistics Table
library(psych)

# Generate descriptive statistics
(shopping_data[, c("Purchase.Amount..USD.", "Age", "Previous.Purchases")])

# Consumer Behavior Insights: Segmentation Analysis
# K-means clustering
set.seed(123)
kmeans_result <- kmeans(shopping_data[, c("Purchase.Amount..USD.", "Age")], centers = 3)
shopping_data$Segment <- as.factor(kmeans_result$cluster)

# Visualize clusters
ggplot(shopping_data, aes(x = Age, y = Purchase.Amount..USD., color = Segment)) +
  geom_point() +
  labs(title = "Customer Segmentation", x = "Age", y = "Purchase Amount (USD)")




