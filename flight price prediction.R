# Set the file path

# Read the CSV file
data <- read.csv("Clean_Dataset.csv")

# View the first few rows of the dataset
head(data)

str(data)



# Convert all character columns to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

# Histogram of Ticket Prices
hist(data$price, 
     breaks = 50, 
     col = "skyblue", 
     border = "black", 
     main = "Distribution of Ticket Prices", 
     xlab = "Price", 
     ylab = "Frequency")
# Bar plot for Airlines
library(ggplot2)
ggplot(data, aes(x = airline)) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Count of Flights by Airline", x = "Airline", y = "Number of Flights") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for Prices by Class
ggplot(data, aes(x = class, y = price, fill = class)) +
  geom_boxplot() +
  labs(title = "Ticket Prices by Class", x = "Class", y = "Price") +
  theme_minimal()
# Calculate average duration by source city
library(dplyr)
avg_duration <- data %>%
  group_by(source_city) %>%
  summarise(avg_duration = mean(duration, na.rm = TRUE))

# Bar plot for average flight duration
ggplot(avg_duration, aes(x = source_city, y = avg_duration, fill = source_city)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average Flight Duration by Source City", x = "Source City", y = "Average Duration (hours)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pie chart for Stops
# Calculate the counts and percentages for stops
stop_counts <- table(data$stops)
stop_percentages <- round(stop_counts / sum(stop_counts) * 100, 1)

# Create the labels with percentages
stop_labels <- paste(names(stop_counts), "(", stop_percentages, "%)", sep = "")

# Create the pie chart
pie(stop_counts, 
    labels = stop_labels, 
    col = c("lightblue", "lightgreen", "pink"), 
    main = "Number of Flights by Stops")

# Load necessary library
library(ggcorrplot)

# Subset the dataset to include only numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Compute the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Plot the correlation matrix
ggcorrplot(correlation_matrix, 
           method = "circle", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"), 
           title = "Correlation Plot", 
           ggtheme = theme_minimal())

# Remove the columns 'X' and 'flight'
data <- data[ , !(names(data) %in% c("X", "flight"))]

# Check the structure of the modified data frame
str(data)

# Print the number of null (NA) values in each column
colSums(is.na(data))

summary(data)

# Load necessary library
library(caTools)

# Set a seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (80% train, 20% test)
split <- sample.split(data$price, SplitRatio = 0.8)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)


# png('CPriceVsDurationScatterPlot.png', width = 800, height = 800)
# ggplot(mapping = aes(x = duration, y = price, color = class), data = train_data) + 
#   geom_point() + geom_smooth(mapping = aes(group = class), color = 'black')
# dev.off()
# 
# png('PriceVsDaysLeftScatterPlot.png', width = 800, height = 800)
# ggplot(mapping = aes(x = days_left, y = price, color = class), data = train_data) + 
#   geom_point() + geom_smooth(mapping = aes(group = class), color = 'black')
# dev.off()
# Load required libraries
library(ggplot2)
library(patchwork)  # Ensure patchwork is loaded

# Generate scatter plots
p1 <- ggplot(mapping = aes(x = duration, y = price, color = class), data = train_data) + 
  geom_point() + 
  geom_smooth(mapping = aes(group = class), color = 'black') + 
  labs(tag = "A")

p2 <- ggplot(mapping = aes(x = days_left, y = price, color = class), data = train_data) + 
  geom_point() + 
  geom_smooth(mapping = aes(group = class), color = 'black') +
  labs(tag = "B")

# Combine plots using patchwork
combined_plot <- p1 + p2

print(combined_plot)


# Train a multiple linear regression model using all predictors
# Load required libraries
set.seed(123)
library(glmnet)  # For Lasso and Ridge regression
library(Metrics) # For calculating MSE
library(caret)   # For data splitting

# Prepare data for models
X_train <- model.matrix(price ~ ., train_data)[, -1]
y_train <- train_data$price
X_test <- model.matrix(price ~ ., test_data)[, -1]
y_test <- test_data$price

### 1. Multiple Linear Regression
model_lm <- lm(price ~ ., data = train_data)
summary(model_lm)

pred_lm_train <- predict(model_lm, train_data)
pred_lm <- predict(model_lm, test_data)
# Define Mean Squared Error (MSE) function
mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

mse_lm <- mse(y_test, pred_lm)
mse_lm_train <- mse(y_train, pred_lm_train)

r2_lm <- cor(y_test, pred_lm)^2
adj_r2_lm_test <- 1-(1-r2_lm)*((nrow(test_data) - 1) / (nrow(test_data) - ncol(X_test) - 1))
print(adj_r2_lm_test)
adj_r2_lm <- summary(model_lm)$adj.r.squared

# Plot Actual vs Predicted for Training Data
png('/Users/gana/Documents/ADV STATS 1/MultLinReg_plots.png',width = 1440, height = 480)



ordfit <- order(pred_lm)
lo <- loess(y_test-pred_lm ~ pred_lm)
lines(pred_lm[ordfit],predict(lo)[ordfit],col = 'red', lwd = 2)

qqnorm(y_test-pred_lm)
qqline(y_test-pred_lm)
dev.off()

### 2. Interaction Model
#  + poly(duration, 3) + poly(days_left,3)

model_interaction <- lm(price ~ (. - duration - days_left + source_city:destination_city + departure_time:arrival_time +  poly(duration, 3) + poly(days_left,3))*class, data = train_data)
summary(model_interaction)
pred_interaction_train <- predict(model_interaction, train_data)
pred_interaction <- predict(model_interaction, test_data)

mse_interaction_train <- mse(y_train, pred_interaction_train)
mse_interaction <- mse(y_test, pred_interaction)

r2_interaction <- cor(y_test, pred_interaction)^2
# adj_r2_interaction <- summary(model_interaction)$adj.r.squared

adj_r2_interaction_test <- 1-(1-r2_interaction)*((nrow(test_data) - 1) / (nrow(test_data) - dim(summary(model_interaction)$coefficients)[1]-1 - 1))
print(mse_interaction_train)
print(mse_interaction)
print(r2_interaction)
print(adj_r2_interaction_test)


png('/Users/gana/Documents/ADV STATS 1/InteractionModel_plots.png',width = 1440, height = 480)
par(mfrow = c(1,3))
plot(test_data$price, pred_interaction, 
     main = "Actual vs Predicted (Model with Several Interaction Terms)", 
     xlab = "Actual Prices", 
     ylab = "Predicted Prices", 
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2, lty = 2) # Adding the reference line (y = x)
grid()

plot(pred_interaction, y_test-pred_interaction,
     main = "Residuals vs Predicted (Test Data) for Interaction Model", 
     xlab = "Predicted Prices", 
     ylab = "Residuals", 
     pch = 19, col = "blue")

ordfitIntcn <- order(pred_interaction)
lo <- loess(y_test-pred_interaction ~ pred_interaction)
lines(pred_interaction[ordfitIntcn],predict(lo)[ordfitIntcn],col = 'red', lwd = 2)

qqnorm(y_test-pred_interaction)
qqline(y_test-pred_interaction)
dev.off()

### 3. Polynomial Regression (using duration as an example for polynomial terms)
model_quad <- lm(price ~ . - duration - days_left + poly(duration, 2) + poly(days_left,2), data = train_data)
summary(model_quad)

pred_quad_train <- predict(model_quad, train_data)
pred_quad <- predict(model_quad, test_data)

mse_quad_train <- mse(y_train, pred_quad_train)
mse_quad <- mse(y_test, pred_quad)
r2_quad <- cor(y_test, pred_quad)^2
adj_r2_quad_test <- 1-(1-r2_quad)*((nrow(test_data) - 1) / (nrow(test_data) - dim(summary(model_quad)$coefficients)[1]-1 - 1))
print(mse_quad_train)
print(mse_quad)
print(r2_quad)
print(adj_r2_quad_test)
# adj_r2_quad <- summary(model_quad)$adj.r.squared

model_poly <- lm(price ~ . - duration - days_left + poly(duration, 3) + poly(days_left,3), data = train_data)
summary(model_poly)

pred_poly_train <- predict(model_poly, train_data)
pred_poly <- predict(model_poly, test_data)

mse_poly_train <- mse(y_train, pred_poly_train)
mse_poly <- mse(y_test, pred_poly)
r2_poly <- cor(y_test, pred_poly)^2
# adj_r2_poly <- summary(model_poly)$adj.r.squared
adj_r2_poly_test <- 1-(1-r2_poly)*((nrow(test_data) - 1) / (nrow(test_data) - dim(summary(model_poly)$coefficients)[1]-1 - 1))
print(mse_poly_train)
print(mse_poly)
print(r2_poly)
print(adj_r2_poly_test)


png('/Users/gana/Documents/ADV STATS 1/PolynomialReg_plots.png',width = 1440, height = 480)
par(mfrow = c(1,3))
plot(test_data$price, pred_poly, 
     main = "Actual vs Predicted (Polynomial Real-valued Terms)", 
     xlab = "Actual Prices", 
     ylab = "Predicted Prices", 
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2, lty = 2) # Adding the reference line (y = x)
grid()

plot(pred_poly, y_test-pred_poly,
     main = "Actual vs Residuals for Model with Polynomial Real-valued terms ", 
     xlab = "Predicted Prices", 
     ylab = "Residuals", 
     pch = 19, col = "blue")

ordfitPoly <- order(pred_poly)
loPoly <- loess(y_test-pred_poly ~ pred_poly)
# lines(pred_poly[ordfitPoly],predict(loPoly)[ordfitPoly],col = 'red', lwd = 2)

qqnorm(y_test-pred_poly)
qqline(y_test-pred_poly)
dev.off()


### 4. Lasso Regression
x_train_lasso <- model.matrix(price ~ .*.,train_data)[,-1]
x_test_lasso <- model.matrix(price~ .*.,test_data)[,-1]
lasso_model <- cv.glmnet(x_train_lasso, y_train, alpha = 1)

x_train_lasso <- model.matrix(price ~ (.-duration-days_left)*(.-duration-days_left) + (poly(duration, 3) + poly(days_left,3))*class,train_data)[,-1]
x_test_lasso <- model.matrix(price~ (.-duration-days_left)*(.-duration-days_left) + (poly(duration, 3) + poly(days_left,3))*class,test_data)[,-1]
lasso_model <- cv.glmnet(x_train_lasso, y_train, alpha = 1)



summary(lasso_model)
pred_lasso_train <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_train_lasso)
pred_lasso <- predict(lasso_model, s = lasso_model$lambda.min, newx = x_test_lasso)

mse_lasso_train <- mse(y_train, pred_lasso_train)
mse_lasso <- mse(y_test, pred_lasso)
r2_lasso <- cor(y_test, pred_lasso)^2
adj_r2_lasso <- 1 - (1 - r2_lasso) * ((nrow(test_data) - 1) / (nrow(test_data) - ncol(x_test_lasso) - 1))
print(mse_lasso_train)
print(mse_lasso)
print(r2_lasso)
print(adj_r2_lasso)



png('/Users/gana/Documents/ADV STATS 1/Lasso_plots.png',width = 1440, height = 480)
par(mfrow = c(1,3))
plot(test_data$price, pred_lasso, 
     main = "Actual vs Predicted (Lasso Regression Model)", 
     xlab = "Actual Prices", 
     ylab = "Predicted Prices", 
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2, lty = 2) # Adding the reference line (y = x)
grid()

plot(pred_lasso, y_test-pred_lasso,
     main = "Residuals vs Predicted (Test Data) for Lasso Regression of All Interactions", 
     xlab = "Predicted Prices", 
     ylab = "Residuals", 
     pch = 19, col = "blue")


ordfitLasso <- order(pred_lasso)
loLasso <- loess(y_test-pred_lasso ~ pred_lasso)
lines(pred_lasso[ordfitLasso],predict(loLasso)[ordfitLasso],col = 'red', lwd = 2)

qqnorm(y_test-pred_lasso)
qqline(y_test-pred_lasso)
dev.off()
### 5. Ridge Regression
ridge_model <- cv.glmnet(x_train_lasso, y_train, alpha = 0)
summary(ridge_model)

pred_ridge_train <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_train_lasso)
pred_ridge <- predict(ridge_model, s = ridge_model$lambda.min, newx = x_test_lasso)

mse_ridge_train <- mse(y_train, pred_ridge_train)
mse_ridge <- mse(y_test, pred_ridge)
r2_ridge <- cor(y_test, pred_ridge)^2
adj_r2_ridge <- 1 - (1 - r2_ridge) * ((nrow(test_data) - 1) / (nrow(test_data) - ncol(x_test_lasso) - 1))
print(mse_ridge_train)
print(mse_ridge)
print(r2_ridge)
print(adj_r2_ridge)




png('/Users/gana/Documents/ADV STATS 1/RidgeReg_plots.png',width = 1440, height = 480)
par(mfrow = c(1,3))
plot(test_data$price, pred_ridge, 
     main = "Actual vs Predicted (Ridge Regression Model)", 
     xlab = "Actual Prices", 
     ylab = "Predicted Prices", 
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2, lty = 2) # Adding the reference line (y = x)
grid()

plot(pred_ridge, y_test-pred_ridge,
     main = "Residuals vs Predicted (Test Data) for Ridge Regression of All Interactions", 
     xlab = "Predicted Prices", 
     ylab = "Residuals", 
     pch = 19, col = "blue")


ordfitRidge <- order(pred_ridge)
loRidge <- loess(y_test-pred_ridge ~ pred_ridge)
lines(pred_ridge[ordfitRidge],predict(loRidge)[ordfitRidge],col = 'red', lwd = 2)

qqnorm(y_test-pred_ridge)
qqline(y_test-pred_ridge)
dev.off()

### 6. Subset Selection
# Generate dummy variables for the training and testing datasets
# X_train <- model.matrix(price ~ airline + source_city + stops + destination_city + class + days_left - 1, data = train_data)
# y_train <- train_data$price
# 
# X_test <- model.matrix(price ~ airline + source_city + stops + destination_city + class + days_left - 1, data = test_data)
# y_test <- test_data$price

X_train <- model.matrix(price ~ ., train_data)[, -1]
y_train <- train_data$price
X_test <- model.matrix(price ~ ., test_data)[, -1]
y_test <- test_data$price


# Selected variables
library(leaps)

# Perform subset selection
# subset_model <- regsubsets(price ~ airline + source_city + stops + destination_city + class + days_left, 
#                            data = train_data, 
#                            nvmax = ncol(train_data) - 1, 
#                            method = "forward") # or "backward", "exhaustive"

subset_model <- regsubsets(price ~ ., 
                           data = train_data, 
                           nvmax = 31, 
                           method = "forward")


# Get the summary of the subset selection
summary_subset <- summary(subset_model)
# Print Adjusted R-Squared for all models
print(summary_subset$bic)


# Identify the best model based on minimum BIC
best_model_size <- which.min(summary_subset$bic)
print(paste("Best model has", best_model_size, "predictors."))
# Get the selected variables for the best model
selected_vars <- names(coef(subset_model, best_model_size))[-1]  # Exclude intercept
print("Selected variables:")
print(selected_vars)

# Subset the training and testing data to include selected variables
# X_train_selected <- X_train[, selected_vars, drop = FALSE]
# X_test_selected <- X_test[, selected_vars, drop = FALSE]
X_train_selected <- X_train
X_test_selected <- X_test




# Fit the linear model
model_subset <- lm(as.formula(paste0("y_train ~ ",paste(selected_vars, collapse = "+"))), data = as.data.frame(X_train_selected))
summary(model_subset)
model_subset_update <- update(model_subset, . ~ . + airlineAirAsia + source_cityChennai + destination_cityMumbai)

# Predictions
pred_subset_train <- predict(model_subset, newdata = as.data.frame(X_train_selected))
pred_subset <- predict(model_subset, newdata = as.data.frame(X_test_selected))

# Define MSE function
mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Compute MSE and R-squared
mse_train <- mse(y_train, pred_subset_train)
mse_test <- mse(y_test, pred_subset)
r2_subset_test <- cor(y_test, pred_subset)^2
adj_r2_subset_test <- 1-(1-r2_subset_test)*((nrow(test_data) - 1) / (nrow(test_data) - length(selected_vars) - 1))
print(mse_train)
print(mse_test)
print(r2_subset_test)
print(adj_r2_subset_test)

adj_r2_subset <- summary(model_subset)$adj.r.squared

# Print metrics
print(paste("Train MSE:", mse_train))
print(paste("Test MSE:", mse_test))
print(paste("Test R-squared:", r2_subset_test))
png('/Users/gana/Documents/ADV STATS 1/ForwardStepWiseReg_plots.png',width = 1440, height = 480)
par(mfrow = c(1,3))
plot(y_test, pred_subset, 
     main = "Actual vs Predicted (Subset Selection)", 
     xlab = "Actual Prices", 
     ylab = "Predicted Prices", 
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2, lty = 2)  # Reference line (y = x)
grid()

plot(pred_subset, y_test-pred_subset,
     main = "Residuals vs Predicted (Forward Step-wise Selection Model)", 
     xlab = "Predicted Prices", 
     ylab = "Residuals", 
     pch = 19, col = "blue")
ordfit <- order(pred_subset)
lo <- loess(y_test-pred_subset ~ pred_subset)
lines(pred_subset[ordfit],predict(lo)[ordfit],col = 'red', lwd = 2)

qqnorm(y_test-pred_subset)
qqline(y_test-pred_subset)
dev.off()

#Performa an Anova  test to compare the baseline model with the subsets model.
anova(model_subset,model_subset_update)

### Summary of Results
results <- data.frame(
  Model = c("Linear Regression", "Interaction Model", "Polynomial Regression", "Lasso Regression", "Ridge Regression", "Subset Selection"),
  Train_MSE = c(mse_lm_train, mse_interaction_train, mse_poly_train, mse_lasso_train, mse_ridge_train, mse_train),
  Test_MSE = c(mse_lm, mse_interaction, mse_poly, mse_lasso, mse_ridge, mse_test),
  R2 = c(r2_lm, r2_interaction, r2_poly, r2_lasso, r2_ridge, r2_subset_test),
  Adjusted_R2 = c(adj_r2_lm, adj_r2_interaction_test, adj_r2_poly_test, adj_r2_lasso, adj_r2_ridge, adj_r2_subset)
)

# Print the updated results
print(results)

#Best model equation
# View the coefficients of the interaction model
coef(model_interaction)


#Cross validation
# Define the control for K-Fold Cross-Validation
set.seed(123)
control <- trainControl(method = "cv", number = 10)  # 10-Fold Cross Validation

### Linear Regression
model_lm <- train(price ~ ., data = train_data, method = "lm", trControl = control)
cat("Linear Regression Accuracy:\n")
print(model_lm$results)
summary(model_lm)


### Interaction Model (Linear Regression with Interaction Terms)
model_interaction <- train(price ~ . + source_city:destination_city + departure_time:arrival_time, 
                           data = train_data, 
                           method = "lm", 
                           trControl = control)
cat("\nInteraction Model Accuracy:\n")
print(model_interaction$results)

### Polynomial Regression
# Example using duration as a polynomial term
model_poly <- train(price ~ poly(duration, 2) + ., data = train_data, method = "lm", trControl = control)
cat("\nPolynomial Regression Accuracy:\n")
print(model_poly$results)

### Lasso Regression
# Prepare data for glmnet
X_train <- model.matrix(price ~ ., train_data)[, -1]
y_train <- train_data$price
lasso_model <- train(
  x = X_train, 
  y = y_train, 
  method = "glmnet", 
  trControl = control, 
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, length = 10))
)
cat("\nLasso Regression Accuracy:\n")
print(lasso_model$results)

### Ridge Regression
ridge_model <- train(
  x = X_train, 
  y = y_train, 
  method = "glmnet", 
  trControl = control, 
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, length = 10))
)
cat("\nRidge Regression Accuracy:\n")
print(ridge_model$results)

# Residuals for Linear Regression
residuals_lm <- y_test - pred_lm

# Residuals for Interaction Model
residuals_interaction <- y_test - pred_interaction

# Residuals for Polynomial Regression
residuals_poly <- y_test - pred_poly

# Residuals for Lasso Regression
residuals_lasso <- y_test - as.vector(pred_lasso) # Convert to vector if needed

# Residuals for Ridge Regression
residuals_ridge <- y_test - as.vector(pred_ridge) # Convert to vector if needed

# Residuals for Subset model
residuals_subset <- y_test - pred_subset

#Residual plots
# Set up a 3x2 grid layout for Q-Q plots
par(mfrow = c(2, 3)) # 2 rows and 3 columns
# Histogram for Linear Regression Residuals
hist(residuals_lm, 
     breaks = 50, 
     col = "skyblue", 
     border = "black", 
     main = "Linear Regression Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency")

# Histogram for Interaction Model Residuals
hist(residuals_interaction, 
     breaks = 50, 
     col = "skyblue", 
     border = "black", 
     main = "Interaction Model Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency")

# Histogram for Polynomial Regression Residuals
hist(residuals_poly, 
     breaks = 50, 
     col = "skyblue", 
     border = "black", 
     main = "Polynomial Regression Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency")

# Histogram for Lasso Regression Residuals
hist(residuals_lasso, 
     breaks = 50, 
     col = "skyblue", 
     border = "black", 
     main = "Lasso Regression Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency")

# Histogram for Ridge Regression Residuals
hist(residuals_ridge, 
     breaks = 50, 
     col = "skyblue", 
     border = "black", 
     main = "Ridge Regression Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency")

# Histogram for Subset Selection Model Residuals
hist(residuals_subset, 
     breaks = 50, 
     col = "skyblue", 
     border = "black", 
     main = "Subset Selection Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency")


# Reset layout to default
par(mfrow = c(1, 1))

