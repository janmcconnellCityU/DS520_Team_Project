# File: q1_explosive_prediction_cuong.R
# Purpose: Model buiding.
# Author: Cuong Vo
# Date: 2025-07-30

# The goal is to classify volcanoes as likely explosive or not
# Consider ration for model:
#   1. Nature of data (analyze through data prepation)
#       - Mixed data type: + Mumeric (Latitude, Longitude, Elevation)
#                          + Categorical (Volcano_Landform, Tectonic_Setting, Dominant_Rock_Type)
#       - Most data are non linear pattern (checknig through data anlysis)
# ... add more reason here later
#
#  Chosen model
# 1. Logistic Regression as base model (Simple and interpretable)
# 2. Decision Tree
# 3. Random Forest
# 4. Gradient Boosted Trees (XGBoost, LightGBM, CatBoost)
# 5. Neural Network (last option to consider as we only have ~8000 data points)


#Install and  Load necessary packages
install.packages("readr")
install.packages("dplyr")
install.packages("caret")
install.packages("glmnet")
install.packages("pROC")
library(readr)
library(dplyr)
library(caret)
library(glmnet)
library(pROC)

# Load the processed dataset
volcano_data_A1 <- read_csv("datasets/processed/volcano_approach1.csv")

# Check structure
#str(volcano_data_A1)
#summary(volcano_data_A1)

# Convert categorical columns to factors
volcano_data_A1 <- volcano_data_A1 %>%
  mutate(
    Explosive = factor(Explosive, labels = c("No", "Yes")),
    Volcano_Landform = as.factor(Volcano_Landform),
    Primary_Volcano_Type = as.factor(Primary_Volcano_Type),
    Tectonic_Setting = as.factor(Tectonic_Setting),
    Dominant_Rock_Type = as.factor(Dominant_Rock_Type),
    Volcanic_Region_Group = as.factor(Volcanic_Region_Group),
    Volcanic_Region = as.factor(Volcanic_Region),
    Country = as.factor(Country)
  )

volcano_logistic <- volcano_data_A1 %>%
  select(
    Explosive,
    Latitude,
    Longitude,
    Elevation,
    Volcano_Landform,
    Primary_Volcano_Type,
    Tectonic_Setting,
    Dominant_Rock_Type
  )

#str(volcano_logistic)
#print("Checking na value: ")
#print(colSums(is.na(volcano_logistic)))

# Clean factor levels for logistic model
volcano_logistic <- volcano_logistic %>%
  mutate(
      Volcano_Landform = factor(make.names(Volcano_Landform)),
      Primary_Volcano_Type = factor(make.names(Primary_Volcano_Type)),
      Tectonic_Setting = factor(make.names(Tectonic_Setting)),
      Dominant_Rock_Type = factor(make.names(Dominant_Rock_Type))
  )
volcano_logistic$Explosive <- relevel(volcano_logistic$Explosive, ref = "Yes")
#======== Simple model=========
print("simple model:")
#Splitting and training model
set.seed(520)
# 1. Split into train/test (80/20)
train_idx <- createDataPartition(volcano_logistic$Explosive
                                  ,p = 0.8, list = FALSE)
train_data <- volcano_logistic[train_idx, ]
test_data  <- volcano_logistic[-train_idx, ]

# 2. Set up 5-fold cross-validation
cv_ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# 3. Train logistic regression with CV
logit_model <- train(
  Explosive ~ Latitude + Longitude + Elevation + Volcano_Landform +
              Primary_Volcano_Type + Tectonic_Setting + Dominant_Rock_Type,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = cv_ctrl,
  metric = "ROC"
)

# 4. Evaluate on hold-out test set
pred <- predict(logit_model, test_data)
print(confusionMatrix(pred, test_data$Explosive))

#========Attemping to tunning===========================================
#Splitting and training mode
# 3. Define hyperparameter grid for glmnet
tune_grid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),       # 0 = Ridge, 1 = LASSO, 0.5 = Elastic Net
  #lambda = 10^seq(-4, 1, length = 50)        # Lambda values from 0.0001 to 10
  # The first run through show that there is no point for training futher
  # than 2.5 as there is no increase in performance.
  # Prefer to Graph/logistic_tuning_ggplot_1.png
  # we zoom in on 0.0001 to 2.5
  #lambda = seq(0.0001, 2.5, length.out = 50)
  # narrow down to 0.001 - 0.2
  lambda = seq(0.001, 0.05, length.out = 30)  # very dense small λ
)

# 3. Train logistic regression with CV
logistic_tuned <- train(
  Explosive ~ ., 
  data = train_data,
  method = "glmnet",       # Regularized Logistic Regression
  family = "binomial",
  metric = "ROC",          # Optimize by ROC AUC
  trControl = cv_ctrl,
  tuneGrid = tune_grid
)

# 4. Review results
png("logistic_tuning.png", width = 1200, height = 800)
plot(logistic_tuned, metric = "ROC")  # Explicitly choose the metric
dev.off()

library(ggplot2)

# Create the plot object
logistic_plot <- ggplot(logistic_tuned) + 
  ggtitle("Logistic Regression Hyperparameter Tuning (Elastic Net)") +
  theme_minimal()

# Save to file
ggsave("logistic_tuning_ggplot.png", logistic_plot, width = 12, height = 8)
#======================================================================
#top_models <- logistic_tuned$results %>%
#  arrange(desc(ROC))  # Sort by highest ROC
#head(top_models, 10)
#second_best <- top_models[2, ]
#third_best  <- top_models[3, ]
# Extract the glmnet object
#glmnet_model <- logistic_tuned$finalModel
# Pick the 2nd and 3rd best hyperparameters
#alpha2  <- second_best$alpha
#lambda2 <- second_best$lambda
#alpha3  <- third_best$alpha
#lambda3 <- third_best$lambda
# 2nd best model predictions
#X_test <- model.matrix(Explosive ~ ., test_data)[, -1]
#pred2 <- predict(glmnet_model, newx = X_test, s = lambda2, type = "response")
# 3rd best model predictions
#pred3 <- predict(glmnet_model, newx = X_test, s = lambda3, type = "response")
# Convert probabilities to classes (threshold = 0.5)
#class_pred2 <- ifelse(pred2 > 0.5, "Yes", "No")
#class_pred3 <- ifelse(pred3 > 0.5, "Yes", "No")
# Confusion Matrix
#print("Confusing matrix for second best: ")
#print(confusionMatrix(as.factor(class_pred2), test_data$Explosive))
#print("Confusing matrix for third best: ")
#print(confusionMatrix(as.factor(class_pred3), test_data$Explosive))
#==================================================================
# 5. Get the second and third best model parameters
# Based on the logistic_tuning_ggplot we can 
# say best consitent best model would be Ridge model
# with lambda around 0.1 - 0.2 => refine to around 0.05
# where there is Moderate Regularization 
# Train the final model
ridge_model <- train(
  Explosive ~ .,
  data = train_data,
  method = "glmnet",
  family = "binomial",
  metric = "ROC",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"  # Needed for CV threshold tuning
  ),
  tuneGrid = data.frame(alpha = 0, lambda = 0.05)
)


# Extract CV predictions and probabilities
cv_preds <- ridge_model$pred
# Print model details
# Use the "Yes" probability column
cv_roc <- roc(response = cv_preds$obs,
              predictor = cv_preds$Yes,
              levels = c("No", "Yes"))

cv_roc <- roc(response = cv_preds$obs,
              predictor = cv_preds$Yes,
              levels = c("No", "Yes"))

# Best threshold using Youden's J
opt_coords <- coords(cv_roc, "best", best.method = "youden")
#print(opt_coords)
threshold <- as.numeric(opt_coords[["threshold"]][1])
#Evaluation
#prep test set
X_test <- model.matrix(Explosive ~ ., test_data)[, -1]
ridge_test_prob <- predict(ridge_model, test_data, type = "prob")
ridge_test_prob <-ridge_test_prob[,"Yes"]
ridge_test_pred <- as.factor(ifelse(ridge_test_prob > threshold, "Yes", "No"))
print("First ridge model")
conf_mat <- confusionMatrix(ridge_test_pred, test_data$Explosive)
print(conf_mat)
print("complete_tuning")
#Conclusion
# Simple model has higher accuracy.
# However it has terrible sensitivy ~ 10% 
# It means it failed to detect when volcano would erupt almost all the time,
# and this is more dangerous.
# Ridge model has lower accuracy but has better sensitivity
# it can detect when volcano erupt explosively better.
