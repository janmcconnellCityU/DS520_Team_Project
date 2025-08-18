# File: q1_explosive_prediction_jan.R
# Purpose: Predict whether a volcano is likely to erupt explosively based on
# type, elevation, and tectonic setting.
# Author: Jan McConnell
# Date: 2025-07-27
# Course: DS520 – Data Mining
# Question: Q1 – Explosive Eruption Prediction

###############################################################################

# Set working directory (adjust path as needed)
setwd("C:/Users/JanMc/Dropbox/Education/_GitHub_coursework/janmcconnellCityU-coursework/DS520 Artificial Intel for Data Sci/TEAM_PROJECT/DS520_Team_Project/") #nolint

###############################################################################

# Load required libraries

# for importing datasets
library(readxl)
# provides functions for filtering, grouping, joining, and transforming data
library(dplyr)
# for generating plots supporting model interpretation/results
library(ggplot2)
# for model training and evaluation
library(caret)
# for data cleaning
library(tidyr)
# for factor manipulation
library(forcats)

# Load datasets
volcanoes <- read_excel("datasets/GVP_Volcano_List_Holocene_202507152349.xlsx")
eruptions <- read_excel("datasets/GVP_Eruption_Search_Result.xlsx")

################################################################################
# DATA EXPLORATION OF ERUPTION DATA
################################################################################

# Preview VEI values in the eruption data
cat("\nSummary of VEI column:\n")
print(summary(eruptions$VEI))

cat("\nUnique VEI values:\n")
print(sort(unique(eruptions$VEI)))

# Check how many rows have missing VEI values
vei_missing <- sum(is.na(eruptions$VEI))
cat("\nNumber of eruptions with missing VEI values:", vei_missing, "\n")

# Convert VEI to numeric (suppress warnings from coercion)
eruptions$VEI <- suppressWarnings(as.numeric(eruptions$VEI))

# Create a binary target variable: 1 = Explosive, 0 = Non-explosive
eruptions$explosive <- ifelse(!is.na(eruptions$VEI) & eruptions$VEI >= 3, 1,
                               ifelse(!is.na(eruptions$VEI), 0, NA))

# Preview new binary variable
cat("\nExplosive eruption label (based on VEI >= 3):\n")
print(table(eruptions$explosive, useNA = "always"))

# Drop rows where the explosive label is NA
eruptions_clean <- eruptions[!is.na(eruptions$explosive), ]

# Confirm the result
cat("\nExplosive eruption label after removing NAs:\n")
print(table(eruptions_clean$explosive, useNA = "always"))

###############################################################################
# DATA EXPLORATION OF VOLCANO DATA
###############################################################################

# Preview column names to confirm availability
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

# Preview sample rows of selected data
cat("\nSample of volcano information:\n")
print(head(volcano_info, 10))

# Check for missing values in each selected column
cat("\nMissing value counts in volcano traits:\n")
print(colSums(is.na(volcano_info)))

# Drop rows with missing tectonic setting
volcano_info <- volcano_info %>%
  filter(!is.na(Tectonic_Setting))

# Confirm result
cat("\nMissing value counts after dropping NAs:\n")
print(colSums(is.na(volcano_info)))

###############################################################################
# MERGE DATA
###############################################################################

# Merge eruption data with volcano traits on Volcano_Number
merged_data <- eruptions_clean %>%
  left_join(volcano_info, by = "Volcano_Number")

# Confirm merge result
cat("\nDimensions of merged dataset:\n")
print(dim(merged_data))

cat("\nPreview of merged dataset:\n")
print(head(merged_data, 10))

###############################################################################
# CREATE SUBSET FOR CLASSIFICATION WITH ONLY FEATURES NEEDED
###############################################################################

# Select only the columns needed for modeling
model_data <- merged_data %>%
  select(Primary_Volcano_Type, `Elevation_(m)`, Tectonic_Setting, explosive)

# Confirm structure and check for any unexpected NAs
cat("\nStructure of model_data:\n")
str(model_data)

cat("\nMissing values in model_data:\n")
print(colSums(is.na(model_data)))

# Drop rows with any missing values
model_data <- model_data %>% drop_na()

# Confirm dimensions and missing value cleanup
cat("\nDimensions after dropping missing values:\n")
print(dim(model_data))

cat("\nMissing values after cleanup:\n")
print(colSums(is.na(model_data)))

################################################################################
# ONE-HOT ENCODING & TRAIN-TEST SPLIT
################################################################################

# One-hot encode categorical variables using model.matrix()
cat("\nGenerating model matrix with one-hot encoding:\n")

# Remove the intercept column and retain explosive column
model_matrix <- model.matrix(~ Primary_Volcano_Type + Tectonic_Setting, data = model_data)[, -1] #nolint

# Combine with numeric and target columns
model_final <- cbind(
  model_matrix,
  Elevation_m = model_data$`Elevation_(m)`,
  explosive = model_data$explosive
)

# Confirm dimensions and column names
cat("\nFinal encoded feature set:\n")
print(dim(model_final))
print(colnames(model_final))

# Convert to data frame
model_final <- as.data.frame(model_final)

# Set seed for reproducibility
set.seed(520)

# Create a 70/30 stratified split
cat("\nCreating 70/30 train-test split (stratified on 'explosive'):\n")
split_index <- createDataPartition(model_final$explosive, p = 0.7, list = FALSE)
train_data <- model_final[split_index, ]
test_data <- model_final[-split_index, ]

# Confirm sizes of split datasets
cat("\nTraining set size:\n")
print(nrow(train_data))
cat("\nTest set size:\n")
print(nrow(test_data))

# Confirm class distribution in both splits
cat("\nClass distribution in training set:\n")
print(table(train_data$explosive))

cat("\nClass distribution in test set:\n")
print(table(test_data$explosive))

################################################################################
# LOGISTIC REGRESSION MODELING & EVALUATION
################################################################################

# Train logistic regression model on training data
logistic_model <- glm(explosive ~ ., data = train_data, family = binomial)

# Summarize the model (optional but informative)
summary(logistic_model)

###############################################################################
# PREDICT PROBABILITIES & EVALUATE CONFUSION MATRIX
###############################################################################

# Predict probabilities on test data
pred_probs <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to binary labels (threshold = 0.5)
pred_labels <- ifelse(pred_probs >= 0.5, 1, 0)

# Confusion matrix
confusionMatrix(
  factor(pred_labels),
  factor(test_data$explosive),
  positive = "1"
)

################################################################################
# LOGISTIC REGRESSION MODEL SUMMARY & INTERPRETATION
################################################################################

# The logistic regression model was trained using explosive eruption (binary)
# as the outcome variable and the volcano's elevation, primary volcano type,
# and tectonic setting as predictors.

# The summary output includes:
# - Coefficients (log-odds estimates)
# - Standard errors
# - z-values
# - p-values
# Significance codes:
#   '***'  p < 0.001 (highly significant)
#   '**'   p < 0.01
#   '*'    p < 0.05
#   '.'    p < 0.1
#   ' '    not significant

# Interpretation of key predictors:
# - Volcano types like "Stratovolcano" or "Caldera(s)" with significant p-values
#   suggest these types are more (or less) likely to be associated with
#   explosive eruptions.
# - Elevation has a positive coefficient and is statistically significant,
#   suggesting that higher-elevation volcanoes are slightly more likely to erupt
#   explosively.
# - Some tectonic settings (e.g., Subduction zones) also appear significant.

# Goodness-of-fit metrics:
# - Null deviance: 5998.0 — baseline model (no predictors)
# - Residual deviance: 5399.0 — shows improvement with predictors
# - AIC: 5393.7 — useful for model comparison; lower AIC = better fit

################################################################################
# MODEL PREDICTIONS & PERFORMANCE METRICS
################################################################################

# The model was used to predict the probability of an explosive eruption on the
# test dataset using `type = "response"`.

# Predictions were converted to binary labels using a 0.5 threshold:
#   - Predicted probability >= 0.5 => class 1 (explosive)
#   - Predicted probability <  0.5 => class 0 (non-explosive)

# Confusion matrix shows:
#   - True Positives (TP): Correctly predicted explosive eruptions
#   - True Negatives (TN): Correctly predicted non-explosive eruptions
#   - False Positives (FP): Non-explosive predicted as explosive
#   - False Negatives (FN): Explosive predicted as non-explosive

# Key evaluation metrics:
# - Accuracy: 0.7592 — overall correct predictions
# - Sensitivity (Recall for class 1): 0.4578 — model's ability to detect
#   explosive eruptions
# - Specificity (Recall for class 0): 0.8846 — model's ability to detect non-
#   explosive eruptions
# - Balanced Accuracy: 0.6712 — average of sensitivity and specificity
# - Detection Rate: 0.2487 — proportion of actual explosive eruptions that were
#   detected
# - McNemar's Test p-value: < 2.2e-16 — statistically significant difference
#   between predicted and actual labels

# Overall:
# - The model performs reasonably well, especially at predicting non-explosive
#   eruptions.
# - Sensitivity is lower than specificity, suggesting the model misses some
#   explosive events.
# - Model could be improved with more features, additional preprocessing, or
#   different algorithms.

################################################################################
# VISUALIZATIONS
################################################################################

# Bar chart of class labels
ggplot(eruptions_clean, aes(x = factor(explosive))) +
  geom_bar(fill = "#0073C2FF") +
  labs(title = "Distribution of Explosive vs Non-Explosive Eruptions",
       x = "Explosive Eruption (1 = Yes, 0 = No)", y = "Count") +
  theme_minimal()

# Save the plot to the visuals folder
ggsave("1_explosive_prediction/visuals/explosive_class_distribution.png", width = 7, height = 5, dpi = 300) #nolint

# Extract coefficients and convert to data frame
coef_df <- as.data.frame(summary(logistic_model)$coefficients)
coef_df$Variable <- rownames(coef_df)
colnames(coef_df) <- c("Estimate", "Std_Error", "z_value", "p_value", "Variable") #nolint

# Remove intercept and sort by absolute value of coefficient
coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]
coef_df <- coef_df %>% arrange(desc(abs(Estimate)))

# Take top 20 most impactful predictors
top_coef <- head(coef_df, 20)

# Dot plot
ggplot(top_coef, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_point(color = "#D55E00", size = 3) +
  coord_flip() +
  labs(title = "Top 20 Logistic Regression Coefficients",
       x = "Predictor",
       y = "Estimate (Log-Odds)") +
  theme_minimal()

# Save plot
ggsave("1_explosive_prediction/visuals/top_logistic_coefficients.png", width = 8, height = 6, dpi = 300) #nolint

# Create confusion matrix table
cm_table <- table(Predicted = pred_labels, Actual = test_data$explosive)

# Convert to data frame
cm_df <- as.data.frame(cm_table)

# Plot confusion matrix as heatmap
ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 6) +
  scale_fill_gradient(low = "lightblue", high = "#0073C2FF") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Actual Label",
       y = "Predicted Label") +
  theme_minimal()

# Save plot
ggsave("1_explosive_prediction/visuals/confusion_matrix_heatmap.png", width = 6, height = 5, dpi = 300) #nolint

# Plot predicted probabilities
ggplot(data.frame(Probability = pred_probs), aes(x = Probability)) +
  geom_histogram(fill = "#009E73", bins = 30, color = "white") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1) + #nolint
  labs(title = "Distribution of Predicted Probabilities",
       x = "Predicted Probability of Explosive Eruption",
       y = "Count") +
  theme_minimal()

# Save plot
ggsave("1_explosive_prediction/visuals/predicted_probabilities_histogram.png", width = 7, height = 5, dpi = 300) #nolint
