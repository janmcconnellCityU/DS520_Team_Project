# File: q2_vei_estimation_jan.R
# Purpose: Estimate the Volcanic Explosivity Index (VEI) of future eruptions
# based on volcano traits and eruption history.
# Author: Jan McConnell
# Date: 2025-07-27
# Course: DS520 – Data Mining
# Question: Q2 – VEI Estimation

################################################################################
# SET WORKING DIRECTORY & PREPARE ENVIRONMENT
################################################################################

# Set working directory (adjust path as needed)
setwd("C:/Users/JanMc/Dropbox/Education/_GitHub_coursework/janmcconnellCityU-coursework/DS520 Artificial Intel for Data Sci/TEAM_PROJECT/DS520_Team_Project/") #nolint

# Define visualization directory for saving plots
viz_dir <- "2_vei_estimation/visuals"

# Load required libraries

# for reading Excel files
library(readxl)
# for data wrangling
library(dplyr)
# for plotting
library(ggplot2)
# for model training and evaluation
library(caret)
# for data cleaning and missing value handling
library(tidyr)
# for string manipulation and factors
library(forcats)

# Load datasets
volcanoes <- read_excel("datasets/GVP_Volcano_List_Holocene_202507152349.xlsx")
eruptions <- read_excel("datasets/GVP_Eruption_Search_Result.xlsx")

################################################################################
# EXPLORATION OF ERUPTION DATA
################################################################################

# Preview VEI column
cat("\nSummary of VEI column:\n")
print(summary(eruptions$VEI))

cat("\nUnique VEI values:\n")
print(sort(unique(eruptions$VEI)))

# Check for missing values in VEI
vei_missing <- sum(is.na(eruptions$VEI))
cat("\nNumber of eruptions with missing VEI values:", vei_missing, "\n")

# Convert VEI to numeric (if not already)
eruptions$VEI <- suppressWarnings(as.numeric(eruptions$VEI))

# Drop rows with missing VEI values
eruptions_clean <- eruptions[!is.na(eruptions$VEI), ]

# Confirm number of observations retained
cat("\nNumber of eruptions retained after removing missing VEI:\n")
print(nrow(eruptions_clean))

# Preview cleaned data
cat("\nSample of cleaned eruption data:\n")
print(head(eruptions_clean, 10))

################################################################################
# EXPLORATION OF VOLCANO DATA
################################################################################

# Preview column names
cat("\nAvailable columns in volcanoes dataset:\n")
print(colnames(volcanoes))

# Select relevant columns for modeling
volcano_info <- volcanoes %>%
  select(
    Volcano_Number,
    Primary_Volcano_Type,
    `Elevation_(m)`,
    Tectonic_Setting
  )

# Preview sample rows
cat("\nSample of selected volcano data:\n")
print(head(volcano_info, 10))

# Check for missing values
cat("\nMissing value counts in selected volcano traits:\n")
print(colSums(is.na(volcano_info)))

# Drop rows with missing values in any selected column
volcano_info <- volcano_info %>%
  filter(!is.na(Primary_Volcano_Type),
         !is.na(`Elevation_(m)`),
         !is.na(Tectonic_Setting))

# Confirm cleanup
cat("\nMissing values after cleanup:\n")
print(colSums(is.na(volcano_info)))

################################################################################
# MERGE ERUPTION AND VOLCANO DATASETS
################################################################################

# Merge cleaned eruption data with volcano trait information
eruption_model_data <- eruptions_clean %>%
  inner_join(volcano_info, by = "Volcano_Number")

# Confirm number of rows after merge
cat("\nNumber of rows in merged dataset:\n")
print(nrow(eruption_model_data))

# Preview merged dataset
cat("\nSample of merged data:\n")
print(head(eruption_model_data, 10))

###############################################################################
# PREPARE FEATURES AND RESPONSE VARIABLE (VEI)
###############################################################################

# Select predictors and response for regression
vei_data <- eruption_model_data %>%
  select(
    VEI,
    Primary_Volcano_Type,
    `Elevation_(m)`,
    Tectonic_Setting
  )

# Confirm structure
cat("\nStructure of regression modeling dataset:\n")
str(vei_data)

# Check for missing values just in case
cat("\nMissing value counts in modeling data:\n")
print(colSums(is.na(vei_data)))

###############################################################################
# ONE-HOT ENCODING & TRAIN-TEST SPLIT
###############################################################################

# One-hot encode categorical variables
dummies <- dummyVars(VEI ~ ., data = vei_data)
vei_encoded <- predict(dummies, newdata = vei_data)
vei_encoded_df <- as.data.frame(vei_encoded)

# Add response variable back to encoded data
vei_encoded_df$VEI <- vei_data$VEI

# Partition data into training (70%) and testing (30%) sets
set.seed(123)
train_index <- createDataPartition(vei_encoded_df$VEI, p = 0.7, list = FALSE)
vei_train <- vei_encoded_df[train_index, ]
vei_test <- vei_encoded_df[-train_index, ]

# Confirm split sizes
cat("\nTraining set size:", nrow(vei_train), "\n")
cat("Testing set size:", nrow(vei_test), "\n")

###############################################################################
# LINEAR REGRESSION MODELING & EVALUATION
###############################################################################

# Fit a linear regression model
lm_model <- lm(VEI ~ ., data = vei_train)

# Summary of the model (optional)
cat("\nLinear Regression Summary:\n")
print(summary(lm_model))

# Predict VEI values on test data
lm_predictions <- predict(lm_model, newdata = vei_test)

# Evaluate model performance
lm_rmse <- sqrt(mean((vei_test$VEI - lm_predictions)^2))
lm_mae <- mean(abs(vei_test$VEI - lm_predictions))
lm_r2 <- 1 - sum((vei_test$VEI - lm_predictions)^2) / sum((vei_test$VEI - mean(vei_test$VEI))^2) #nolint

# Output evaluation metrics
cat("\nLinear Regression Evaluation Metrics:\n")
cat("RMSE:", round(lm_rmse, 3), "\n")
cat("MAE :", round(lm_mae, 3), "\n")
cat("R²  :", round(lm_r2, 3), "\n")

# Plot: Actual vs Predicted VEI
library(ggplot2)
ggplot(data.frame(Actual = vei_test$VEI, Predicted = lm_predictions),
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "#0073C2") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Linear Regression: Actual vs Predicted VEI",
       x = "Actual VEI",
       y = "Predicted VEI") +
  theme_minimal()

# Save plot
ggsave("2_vei_estimation/visuals/lm_actual_vs_predicted.png", width = 7, height = 5, dpi = 300) #nolint

# lm_actual_vs_predicted.png
# This scatter plot compares actual VEI values (x-axis) with predicted values
# from the linear regression model (y-axis). The dashed red line represents
# perfect prediction (i.e., predicted VEI equals actual VEI).

# Key observations:
# - Predictions are heavily clustered between VEI 1.5 and 2.5, indicating
#   the model is regressing toward the mean.
# - Higher actual VEI values (e.g., 4, 5, 6) are systematically underpredicted.
# - Lower actual VEI values (e.g., 0, 1) are often overpredicted.
# - The linear model does not adequately capture the discrete, ordinal nature
#   of the VEI variable.

# Conclusion:
# While the linear regression provides a useful baseline, its inability to
# model nonlinear or categorical patterns in VEI suggests the need for more
# flexible models. To address this limitation, we will explore decision tree
# and random forest regression in the next section.

################################################################################
# DECISION TREE REGRESSION MODELING & EVALUATION
################################################################################

# Ensure valid column names (especially after one-hot encoding)
colnames(vei_train) <- make.names(colnames(vei_train))
colnames(vei_test) <- make.names(colnames(vei_test))

# Train a decision tree model using rpart
library(rpart)
dt_model <- rpart(VEI ~ ., data = vei_train, method = "anova")

# Predict VEI values on test data
dt_predictions <- predict(dt_model, newdata = vei_test)

# Evaluate model performance
dt_rmse <- sqrt(mean((vei_test$VEI - dt_predictions)^2))
dt_mae <- mean(abs(vei_test$VEI - dt_predictions))
dt_r2 <- 1 - sum((vei_test$VEI - dt_predictions)^2) / sum((vei_test$VEI - mean(vei_test$VEI))^2) #nolint

# Output evaluation metrics
cat("\nDecision Tree Regression Evaluation Metrics:\n")
cat("RMSE:", round(dt_rmse, 3), "\n")
cat("MAE :", round(dt_mae, 3), "\n")
cat("R²  :", round(dt_r2, 3), "\n")

# Plot: Actual vs Predicted VEI (Decision Tree)
ggplot(data.frame(Actual = vei_test$VEI, Predicted = dt_predictions),
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "#E69F00") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Decision Tree: Actual vs Predicted VEI",
       x = "Actual VEI",
       y = "Predicted VEI") +
  theme_minimal()

# Save plot
ggsave("2_vei_estimation/visuals/dt_actual_vs_predicted.png", width = 7, height = 5, dpi = 300) #nolint

# dt_actual_vs_predicted.png
# This scatter plot compares actual VEI values (x-axis) with predicted values
# from the decision tree regression model (y-axis). The red dashed line
# represents perfect prediction (y = x).

# Key observations:
# - The decision tree model outputs a limited number of discrete prediction
#   levels, leading to horizontal lines at specific predicted VEI values.
# - Many predictions are concentrated around VEI ≈ 0.7 and VEI ≈ 2.2, suggesting
#   the model may be overly simplistic or not fully capturing complex patterns.
# - High VEI values (e.g., 4, 5, 6) are consistently underpredicted.

# Conclusion:
# While the decision tree introduces some nonlinearity compared to linear
# regression, it still struggles to capture the true variation in VEI. A more
# robust, ensemble-based model may help improve prediction performance. We will
# next explore a random forest approach.

################################################################################
# RANDOM FOREST REGRESSION MODELING & EVALUATION
################################################################################

# Load required library
library(randomForest)

# Train a random forest regression model
set.seed(123)
rf_model <- randomForest(VEI ~ ., data = vei_train, ntree = 500, importance = TRUE) #nolint

# Print model summary
cat("\nRandom Forest Model Summary:\n")
print(rf_model)

# Predict VEI on the test set
rf_predictions <- predict(rf_model, newdata = vei_test)

# Evaluate model performance
rf_rmse <- sqrt(mean((vei_test$VEI - rf_predictions)^2))
rf_mae <- mean(abs(vei_test$VEI - rf_predictions))
rf_r2 <- 1 - sum((vei_test$VEI - rf_predictions)^2) / sum((vei_test$VEI - mean(vei_test$VEI))^2) #nolint

# Output evaluation metrics
cat("\nRandom Forest Evaluation Metrics:\n")
cat("RMSE:", round(rf_rmse, 3), "\n")
cat("MAE :", round(rf_mae, 3), "\n")
cat("R²  :", round(rf_r2, 3), "\n")

# Plot: Actual vs Predicted VEI for Random Forest
ggplot(data.frame(Actual = vei_test$VEI, Predicted = rf_predictions),
       aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "#E69F00") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Random Forest: Actual vs Predicted VEI",
       x = "Actual VEI",
       y = "Predicted VEI") +
  theme_minimal()

# Save plot
ggsave("2_vei_estimation/visuals/rf_actual_vs_predicted.png", width = 7, height = 5, dpi = 300) #nolint

# rf_actual_vs_predicted.png
# This scatter plot compares actual VEI values (x-axis) with predicted values
# from the random forest model (y-axis). The dashed red line indicates perfect
# prediction. Points clustering along this line indicate higher model accuracy.

# Key observations:
# - Predictions are more dispersed than linear regression, suggesting that the
#   model is capturing more variation in VEI levels.
# - However, the model still regresses toward the mean (centered around VEI 2),
#   and tends to underpredict higher VEI values (e.g., 5 or 6).
# - Compared to decision trees, random forest reduces the overfitting and
#   improves generalization.

# Conclusion:
# The random forest model offers a stronger and more flexible approach to VEI
# estimation than linear regression or a single decision tree. While still not
# ideal for predicting very high or very low VEI values, it provides a more
# reliable basis for estimating eruption severity based on volcano traits.

################################################################################
# MODEL COMPARISON SUMMARY TABLE
################################################################################

# Combine evaluation metrics into a single data frame
model_comparison <- data.frame(
  Model = c("Linear Regression", "Decision Tree", "Random Forest"),
  RMSE = round(c(lm_rmse, dt_rmse, rf_rmse), 3),
  MAE  = round(c(lm_mae, dt_mae, rf_mae), 3),
  R2   = round(c(lm_r2, dt_r2, rf_r2), 3)
)

# Print model comparison
cat("\nModel Performance Comparison:\n")
print(model_comparison)

################################################################################
# VARIABLE IMPORTANCE PLOT (RANDOM FOREST)
################################################################################

# Get variable importance
rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(Feature = rownames(rf_importance),
                               Importance = rf_importance[, "%IncMSE"])

# Sort by importance
rf_importance_df <- rf_importance_df %>%
  arrange(desc(Importance))

# Plot variable importance
ggplot(rf_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) + #nolint
  geom_bar(stat = "identity", fill = "#56B4E9") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance",
       x = "Feature",
       y = "% Increase in MSE if Removed") +
  theme_minimal()

# Save plot
ggsave("2_vei_estimation/visuals/rf_variable_importance.png", width = 7, height = 5, dpi = 300) #nolint

################################################################################
# Model Comparison and Feature Importance Summary
################################################################################

# After training three regression models (Linear Regression, Decision Tree, and
# Random Forest) to predict VEI based on elevation, volcano type, and tectonic
# setting, we evaluated # their performance using RMSE, MAE, and R² metrics.

# Results Summary:
# - Random Forest achieved the best performance with the lowest RMSE (0.967),
#   lowest MAE (0.692), and highest R² (0.281).
# - Linear Regression had slightly higher RMSE (1.046) and MAE (0.774) compared
#   to Decision Tree.
# - Decision Tree had the weakest performance overall (highest RMSE and lowest
#   R²).

# Variable Importance:
# - Random Forest variable importance shows that elevation (X.Elevation..m..) is
#   the most influential feature in predicting VEI, followed by subduction zone
#   types and primary volcano types like Shield and Caldera.
# - This highlights that physical geography (elevation) and tectonic
#   characteristics contribute more to eruption magnitude than volcano shape
#   alone.

# Recommendation:
# - Random Forest is the preferred model for VEI prediction due to better
#   generalization.
# - Elevation and tectonic zone data should be prioritized in further hazard
#   modeling.

# CONCLUSION
# We built three regression models to estimate the Volcanic Explosivity Index
# (VEI) of future eruptions using volcano characteristics and eruption history:
# - Linear Regression
# - Decision Tree
# - Random Forest
#
# Among these, the Random Forest model performed the best, achieving:
# - RMSE: 0.967
# - MAE : 0.692
# - R²  : 0.281
#
# The most important predictor was elevation, followed by tectonic setting and
# volcano type. While model performance leaves room for improvement, the results
# demonstrate that volcano characteristics do have predictive power for
# estimating eruption severity (VEI).
#
# These findings support the feasibility of using volcano traits to anticipate
# eruption intensity. The VEI estimates generated here will be incorporated into
# a broader hazard assessment in Question 3, which will also consider eruption
# frequency and population exposure to identify volcanoes posing the greatest
# risk.
