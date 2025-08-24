# File: q2_vei_regression_run.R
# Purpose: Predict VEI (0–8) from volcano traits using regression models
# Author: Cuong Vo
# Date: 2025-08-17

# ======================= Packages =======================
req_pkgs <- c("readr","dplyr","forcats","caret","ggplot2","ranger","xgboost","MASS","tibble")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[,"Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)

library(readr); library(dplyr); library(forcats); library(caret); library(ggplot2)
library(ranger); library(xgboost); library(MASS); library(tibble)

# ======================= Output Folders =================
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# ======================= Load Dataset ===================
vei_df <- read_csv("datasets/processed/vei_estimate_data.csv", show_col_types = FALSE)

# Ensure VEI is numeric and valid
vei_df <- vei_df %>%
  mutate(VEI = as.numeric(VEI)) %>%
  filter(!is.na(VEI), VEI >= 0, VEI <= 8)

# Categorical columns
cat_cols <- c("Volcano_Landform","Primary_Volcano_Type","Tectonic_Setting",
              "Dominant_Rock_Type","Volcanic_Region_Group","Volcanic_Region","Country")

# Numeric columns
num_cols <- c("Latitude","Longitude","Elevation")

# Convert categoricals
vei_df <- vei_df %>%
  mutate(across(all_of(cat_cols), ~ fct_na_value_to_level(as.factor(.), "Unknown")))

# Final feature set
feature_cols <- c(num_cols, cat_cols)
fmla <- as.formula(paste("VEI ~", paste(feature_cols, collapse = " + ")))

# ======================= Train/Test Split ===============
set.seed(520)
idx <- createDataPartition(vei_df$VEI, p = 0.8, list = FALSE)
train_data <- vei_df[idx, ]
test_data  <- vei_df[-idx, ]
cat(sprintf("Train: %d | Test: %d\n", nrow(train_data), nrow(test_data)))

# ======================= Weights ========================
vei_freq <- table(train_data$VEI)
vei_wts  <- 1 / as.numeric(vei_freq)
names(vei_wts) <- names(vei_freq)
train_w  <- vei_wts[as.character(train_data$VEI)]

# # ======================= Helpers ========================
# within1_acc <- function(y_true, y_pred) mean(abs(round(y_pred) - y_true) <= 1)
# qwk <- function(y_true, y_pred, min_rat = 0, max_rat = 8) {
#   y_true <- as.integer(y_true); y_pred <- as.integer(round(y_pred))
#   L <- max_rat - min_rat + 1
#   O <- table(factor(y_true, min_rat:max_rat), factor(y_pred, min_rat:max_rat))
#   O <- as.matrix(O)
#   r <- rowSums(O); c <- colSums(O); E <- outer(r, c) / sum(r)
#   W <- outer(1:L, 1:L, function(i,j) ((i-j)^2) / ((L-1)^2))
#   1 - sum(W * O) / sum(W * E)
# }
cv_reg <- trainControl(method = "cv", number = 5)

# #======================= 1) Random Forest ===============
# print("Training RF regression... ")
# rf_model <- caret::train(
#   fmla,
#   data = train_data,
#   method = "ranger",
#   trControl = cv_reg,
#   metric = "RMSE",
#   tuneLength = 7,
#   importance = "permutation",
#   num.trees = 500,
#   weights = train_w   
# )
# rf_pred <- predict(rf_model, test_data)
# print("============================")
# # ===== RF: metrics, top-10 importance table + plot =====
# rf_mae   <- mean(abs(rf_pred - test_data$VEI))
# rf_rmse  <- sqrt(mean((rf_pred - test_data$VEI)^2))
# rf_w1    <- within1_acc(test_data$VEI, rf_pred)
# rf_qwk   <- qwk(test_data$VEI, rf_pred)
# rf_spear <- suppressWarnings(cor(test_data$VEI, rf_pred, method = "spearman"))

# rf_imp_tbl <- varImp(rf_model)$importance |>
#   tibble::rownames_to_column("Feature") |>
#   dplyr::arrange(dplyr::desc(Overall)) |>
#   dplyr::slice(1:10)
# readr::write_csv(rf_imp_tbl, "outputs/tables/rf_top10_importance.csv")

# ggplot(rf_imp_tbl, aes(x = reorder(Feature, Overall), y = Overall)) +
#   geom_col() +
#   coord_flip() +
#   labs(title = "RF Permutation Importance (Top 10)", x = "Feature", y = "Importance") +
#   theme_minimal()
# ggsave("outputs/plots/rf_top10_importance.png", width = 7, height = 5, dpi = 150)

# png("outputs/plots/rf_tuning.png", 1200, 800); plot(rf_model); dev.off()

# # ======================= 2) XGBoost (regression) ===================
# print("Training XGBoost regression... ")
# xgb_grid <- expand.grid(
#   nrounds = 300,
#   max_depth = c(3, 5, 7),
#   eta = c(0.05, 0.10),
#   gamma = 0,
#   colsample_bytree = 0.8,
#   min_child_weight = c(1, 3),
#   subsample = 0.8
# )

# xgb_model <- caret::train(
#   fmla,
#   data = train_data,
#   method = "xgbTree",
#   trControl = cv_reg,
#   tuneGrid = xgb_grid,
#   metric = "RMSE",
#   weights = train_w
# )
# xgb_pred  <- predict(xgb_model, test_data)

# xgb_mae   <- mean(abs(xgb_pred - test_data$VEI))
# xgb_rmse  <- sqrt(mean((xgb_pred - test_data$VEI)^2))
# xgb_w1    <- within1_acc(test_data$VEI, xgb_pred)
# xgb_qwk   <- qwk(test_data$VEI, xgb_pred)
# xgb_spear <- suppressWarnings(cor(test_data$VEI, xgb_pred, method = "spearman"))

# xgb_imp_tbl <- varImp(xgb_model)$importance |>
#   tibble::rownames_to_column("Feature") |>
#   dplyr::arrange(dplyr::desc(Overall)) |>
#   dplyr::slice(1:10)
# readr::write_csv(xgb_imp_tbl, "outputs/tables/xgb_top10_importance.csv")

# ggplot(xgb_imp_tbl, aes(x = reorder(Feature, Overall), y = Overall)) +
#   geom_col() +
#   coord_flip() +
#   labs(title = "XGBoost Importance (Top 10)", x = "Feature", y = "Importance") +
#   theme_minimal()
# ggsave("outputs/plots/xgb_top10_importance.png", width = 7, height = 5, dpi = 150)

# png("outputs/plots/xgb_tuning.png", 1200, 800); plot(xgb_model); dev.off()

# # ======================= 3) Ordinal baseline (polr, no CV) ========
# print("Training Ordinal (polr) baseline (no CV)... ")
# train_ord <- train_data[, c("VEI", feature_cols)]
# test_ord  <- test_data[,  c("VEI", feature_cols)]

# train_ord$VEI_ord <- factor(train_ord$VEI, ordered = TRUE)
# test_ord$VEI_ord  <- factor(test_ord$VEI,  ordered = TRUE, levels = levels(train_ord$VEI_ord))

# # Fit once on the whole training set (no resampling)
# ord_model <- MASS::polr(VEI_ord ~ ., data = train_ord[, c("VEI_ord", feature_cols)], Hess = TRUE)
# saveRDS(ord_model, "outputs/ord_vei_model.rds")

# # Predict classes on the full test set
# ord_pred_fac <- predict(ord_model, newdata = test_ord)
# ord_pred <- suppressWarnings(as.numeric(as.character(ord_pred_fac)))
# if (any(is.na(ord_pred))) {
#   ord_levels <- suppressWarnings(as.numeric(levels(ord_pred_fac)))
#   ord_pred <- ord_levels[as.integer(ord_pred_fac)]
# }

# # Metrics
# ord_mae   <- mean(abs(ord_pred - test_ord$VEI))
# ord_rmse  <- sqrt(mean((ord_pred - test_ord$VEI)^2))
# ord_w1    <- within1_acc(test_ord$VEI, ord_pred)
# ord_qwk   <- qwk(test_ord$VEI, ord_pred)
# ord_spear <- suppressWarnings(cor(test_ord$VEI, ord_pred, method = "spearman"))

# Save best parameters and CV results
# ==================== Save result ======================================
# RF best params + all CV results
# readr::write_csv(rf_model$results, "outputs/tables/rf_cv_results.csv")
# writeLines(capture.output(rf_model$bestTune), "outputs/tables/rf_best_params.txt")

# # XGB best params + all CV results
# readr::write_csv(xgb_model$results, "outputs/tables/xgb_cv_results.csv")
# writeLines(capture.output(xgb_model$bestTune), "outputs/tables/xgb_best_params.txt")

# For polr, caret also produces CV results, but no hyperparams to tune
# readr::write_csv(ord_model$results, "outputs/tables/ord_cv_results.csv")

# ======================= Summary table =============================
# metrics <- tibble::tibble(
#   Model     = c("RandomForest (ranger)","XGBoost (reg)","Ordinal (polr)"),
#   MAE       = c(rf_mae, xgb_mae, ord_mae),
#   RMSE      = c(rf_rmse, xgb_rmse, ord_rmse),
#   Within_1  = c(rf_w1, xgb_w1, ord_w1),
#   QWK       = c(rf_qwk, xgb_qwk, ord_qwk),
#   Spearman  = c(rf_spear, xgb_spear, ord_spear)
# )
# metrics <- tibble::tibble(
#   Model     = c("RandomForest (ranger)","XGBoost (reg)"),
#   MAE       = c(rf_mae, xgb_mae),
#   RMSE      = c(rf_rmse, xgb_rmse),
#   Within_1  = c(rf_w1, xgb_w1),
#   QWK       = c(rf_qwk, xgb_qwk),
#   Spearman  = c(rf_spear, xgb_spear)
# )
# readr::write_csv(metrics, "outputs/tables/vei_regression_summary.csv")
# print(metrics)

# # ======================= Pred vs Actual plots ======================
# plot_pred <- function(actual, pred, title, file) {
#   df <- data.frame(Actual = actual, Pred = pred)
#   p <- ggplot(df, aes(Actual, Pred)) +
#     geom_point(alpha = 0.5) +
#     geom_abline(slope = 1, intercept = 0, linetype = 2) +
#     labs(title = title, x = "Actual VEI", y = "Predicted VEI") +
#     theme_minimal()
#   ggsave(file, p, width = 8, height = 6, dpi = 150)
# }
# plot_pred(test_data$VEI, rf_pred,  "RF: Pred vs Actual VEI",  "outputs/plots/rf_pred_vs_actual.png")
# plot_pred(test_data$VEI, xgb_pred, "XGB: Pred vs Actual VEI", "outputs/plots/xgb_pred_vs_actual.png")
# #plot_pred(test_data$VEI, ord_pred, "POLR: Pred vs Actual VEI","outputs/plots/ord_pred_vs_actual.png")

# cat("\nArtifacts saved:\n",
#     "- outputs/tables/vei_regression_summary.csv\n",
#     "- outputs/tables/rf_top10_importance.csv, xgb_top10_importance.csv\n",
#     "- outputs/plots/rf_top10_importance.png, xgb_top10_importance.png\n",
#     "- outputs/plots/*_tuning.png, *_pred_vs_actual.png\n",
#     "- outputs/rf_vei_model.rds, outputs/xgb_vei_model.rds, outputs/ord_vei_model.rds\n")
#Above code for hyperparamter tunning
#to train only the best model can run the code below 
# Conclusion so far
## ======================= Model Tuning Summary =======================
# Random Forest (RF)
# - Hyperparameters tuned:
#     * mtry: 2 to 241
#     * splitrule: {variance, extratrees}
#     * min.node.size: fixed at 5
#     * num.trees: fixed at 500
# - Best model:
#     * mtry = 41
#     * splitrule = variance
#     * min.node.size = 5
#   -> Lowest CV RMSE ≈ 1.23, MAE ≈ 0.90
#
# XGBoost (XGB)
# - Hyperparameters tuned:
#     * eta (learning rate): {0.05, 0.1}
#     * max_depth: {3, 5, 7}
#     * min_child_weight: {1, 3}
#     * nrounds: fixed at 300
#     * subsample = 0.8, colsample_bytree = 0.8
#     * gamma = 0
# - Best model:
#     * eta = 0.1
#     * max_depth = 3
#     * min_child_weight = 1
#     * nrounds = 300
#   -> Lowest CV RMSE ≈ 1.53, MAE ≈ 1.23
#
# Both RF and XGB consistently identified Longitude, Latitude, and Elevation
# as the strongest predictors of VEI, with tectonic setting and magma type
# providing additional explanatory power.
# =====================================================================
