# File: q3_risk_profiling_jan.R
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
install.packages("janitor")
library(readr)
library(dplyr)
library(caret)
library(janitor)

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

str(volcano_logistic)
print("Checking na value: ")
print(colSums(is.na(volcano_logistic)))

# Clean factor levels for logistic model
volcano_logistic <- volcano_logistic %>%
  mutate(
      Volcano_Landform = factor(make.names(Volcano_Landform)),
      Primary_Volcano_Type = factor(make.names(Primary_Volcano_Type)),
      Tectonic_Setting = factor(make.names(Tectonic_Setting)),
      Dominant_Rock_Type = factor(make.names(Dominant_Rock_Type))
  )
volcano_logistic$Explosive <- relevel(volcano_logistic$Explosive, ref = "Yes")
#==========================================================
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