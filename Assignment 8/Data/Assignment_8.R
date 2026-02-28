## ============================================================
## Assignment 8
## Wine Purchase Analysis
## Logistic Regression on WINE Dataset
## Response Variable: Purchase (0 = No, 1 = Yes)
## ============================================================

# Set-up environment ------------------------------------------------------
# Clear environment
rm(list = ls())

# Load libraries
library(corrplot)
library(MASS)
library(pROC)
library(tidyverse)
library(car)
library(readxl)
# install.packages("fastDummies") # Install if needed
library(fastDummies)
# install.packages("Metrics") # Install if needed
library(Metrics)
library(nnet)

# Import dataset
wine_data <- readxl::read_xlsx("wine.xlsx")

# Create output directory
output_dir <- "Output"

if (!dir.exists(output_dir)) {
  dir.create(path = paste(dirname(getwd()), "/Output", sep = ""))
} else {
  print("Directory already exists!")
}
# Output path
output_path <- file.path(dirname(getwd()), "/Output", sep = "")

# EDA ------------------------------------------------------------
# For this analysis, Purchase is used as the binary response variable
# indicating whether wine cases were purchased (1) or not (0).
# The goal is predict the number of wine cases ordered based upon the wine characteristics.

## Overview of dataset ##
cat("---Dataset overview: ---\n")
str(wine_data)

## Remove INDEX column ##
wine_data <- wine_data %>% select(-INDEX)

## Create TARGET_F variable ##
TARGET_F <- "Purchase"

## Find any missing values ##
cat("--- Find missing values: ---\n")
sapply(wine_data, function(x) sum(is.na(x)))
cat(sprintf("There is a total of %d missing values across the wine dataset.", sum(is.na(wine_data))))

# INTERPRETATION: There are a good amount of datasets, requiring imputation.
# For continuous variables, we will use the median to impute missing values.
# For discrete variables, we will use the mode to impute missing values.
#
# RECOMMENDATION: 

## Make discrete columns discrete ##
categorical_cols <- c("Cases", "STARS", "LabelAppeal", "AcidIndex")
wine_data[categorical_cols] <- lapply(wine_data[categorical_cols], as.factor)

cat(sprintf("We changed %d columns to a factor data type.", sum(sapply(wine_data, is.factor))))

## Split by data types ##
# Discrete variables
cat_df_missing <- wine_data %>%
  select(where(is.factor)) %>%
  keep(~any(is.na(.)))

# Print results
head(cat_df_missing)

# Continuous variables
continuous_vars <- names(wine_data[, !names(wine_data) %in% c(categorical_cols, TARGET_F)])
cont_df_missing <- wine_data %>%
  select(where(is.numeric)) %>%
  keep(~any(is.na(.)))

# Print results
head(cont_df_missing)

# Print results
head(cont_df_missing)

## INTERPRETATION: Of the 5 discrete variables in the wine dataset, only one containing missing values. 
## The missing values are only in the STAR column and this variable will be imputed with the mode value.
## There are 7 continuous variables in the wine dataset with missing values.
## These missing values will be imputed with the median value as the mean value is sensitive to extreme values.

## Fill with mode ##
# Create copy data
wine_data_imputed <- wine_data

# Create mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

wine_data_imputed$STARS[is.na(wine_data_imputed$STARS)] <- Mode(wine_data_imputed$STARS)

# Print results
head(wine_data_imputed$STARS)

## Fill with median ##
wine_data_imputed <- wine_data_imputed %>%
  mutate(across(all_of(continuous_vars), ~ replace(., is.na(.), median(., na.rm = TRUE))))

# Print results
head(wine_data_imputed)

## Check for missing values ##
cat("--- Check for missing values: ---\n")
if (sum(is.na(wine_data_imputed)) > 0) {
  cat("The are", sum(is.na(wine_data_imputed)), "missing values remaining in the wine dataset.\n")
} else {
  cat("We removed all missing values from the wine dataset and can proceed with the EDA!\n")
}

## Correlation ##
# Calculate correlations
wine_numeric <- wine_data_imputed %>%
  mutate(across(where(is.factor), as.numeric))  # convert ALL factors including Purchase

cor_matrix <- cor(wine_numeric)

purchase_correlations <- cor_matrix[, "Purchase"] %>%
  sort(decreasing = TRUE) %>%
  as.data.frame()

names(purchase_correlations) <- "Correlation"

cat("\nCorrelations with Purchase (sorted):\n\n")
print(purchase_correlations, digits = 3)

# Identify strongest predictors
cat("\n\nTOP 5 POSITIVE PREDICTORS (associated with mortality):\n")
top_pos <- purchase_correlations %>%
  filter(row.names(.) != "STA") %>%
  filter(Correlation > 0) %>%
  arrange(desc(Correlation)) %>%
  head(5)
print(top_pos, digits = 3)

cat("\n\nTOP 5 NEGATIVE PREDICTORS (associated with mortality):\n")
top_neg <- purchase_correlations %>%
  filter(row.names(.) != "STA") %>%
  filter(Correlation < 0) %>%
  arrange(desc(Correlation)) %>%
  head(5)
print(top_neg, digits = 3)

# INTERPRETATION: `Cases` has by far the strongest prediction of purchase status, though
# this is likely because the number of cases bought is a direct consequence of purchasing
# a case of wine. `STAR` is the next highest predictor with a weak linear relationship to purchase.
# RECOMMENDATION: We should exclude `Cases` from any predictive model, or at minimum scrutinized carefully.

## Descriptive statistics ##
detailed_stats <- wine_data_imputed %>%
  select(all_of(continuous_vars)) %>%
  summarise(across(everything(), list(
    Count = ~ n(),
    Mean = ~ mean(., na.rm = TRUE),
    SD = ~ sd(., na.rm = TRUE),
    Min = ~ min(., na.rm = TRUE),
    Median = ~ median(., na.rm = TRUE),
    Max = ~ max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)") %>%
  arrange(Variable)

print(detailed_stats, digits = 3)

## Factor variable distributions ##
cat("\n=== FACTOR VARIABLE FREQUENCY TABLES ===\n")
for (var in categorical_cols) {
  cat("\n---", var, "---\n")
  tbl <- table(wine_data_imputed[[var]])
  tbl_pct <- prop.table(tbl) * 100
  combined <- rbind(Count = tbl, Percent = round(tbl_pct, 1))
  print(combined)
}

## Detect outliers ##
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- x[x < lower_bound | x > upper_bound]
  
  list(
    Q1 = Q1,
    Q3 = Q3,
    IQR = IQR,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    outliers = outliers,
    n_outliers = length(outliers)
  )
}

for(var in continuous_vars) {
  cat("\n", var, ":\n", sep="")
  outlier_info <- detect_outliers(wine_data_imputed[[var]])
  
  cat("  IQR Range: [", sprintf("%.1f", outlier_info$Q1), ", ",
      sprintf("%.1f", outlier_info$Q3), "], IQR = ",
      sprintf("%.1f", outlier_info$IQR), "\n", sep="")
  cat("  Outlier bounds: < ", sprintf("%.1f", outlier_info$lower_bound),
      " or > ", sprintf("%.1f", outlier_info$upper_bound), "\n", sep="")
  cat("  Number of outliers: ", outlier_info$n_outliers,
      "", sprintf("%.1f%%", outlier_info$n_outliers/nrow(df)*100), "\n", sep="")
  
  if(outlier_info$n_outliers > 0) {
    cat("  Outlier values:", paste(sort(outlier_info$outliers), collapse=", "), "\n")
  }
}

# INTERPRETATION: 
#
# RECOMMENDATION: 

## Windsorize extreme values ##
winsorize_vars <- function(data, cols) {
  data_clean <- data
  
  for(col in cols) {
    Q1  <- quantile(data_clean[[col]], 0.25, na.rm = TRUE)
    Q3  <- quantile(data_clean[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    
    n_capped <- sum(data_clean[[col]] < lower | data_clean[[col]] > upper, na.rm = TRUE)
    
    data_clean[[col]] <- pmax(pmin(data_clean[[col]], upper), lower)
    
    cat(sprintf("%-22s | Lower: %7.2f | Upper: %7.2f | Values capped: %d\n",
                col, lower, upper, n_capped))
  }
  return(data_clean)
}

cat("=== WINSORIZATION SUMMARY ===\n\n")
wine_cleaned <- wine_data_imputed
wine_cleaned[continuous_vars] <- winsorize_vars(wine_data_imputed[continuous_vars], 
                                                continuous_vars)

cat("\nOriginal data:", nrow(wine_data_imputed), 
    "| Cleaned data:", nrow(wine_cleaned),
    "| Rows retained: 100%\n")

## INTERPRETATION:
##
## RECOMMENDATION:

# Visualizations ----------------------------------------------------------
## Response variable Barplot ##
ggplot(data = wine_cleaned, aes(x = factor(Purchase, labels = c("Not Purchased", "Purchased")), 
       fill = factor(Purchase, labels = c("Not Purchased", "Purchased")))) +
  geom_bar(color = "black", width = 0.6) +
  geom_text(aes(label = paste0(after_stat(count), "\n(",
                               scales::percent(after_stat(count)/sum(after_stat(count)), accuracy = 0.1), ")")),
            stat = "count", vjust = -0.4, size = 5) +
  scale_fill_manual(values = c("Not Purchased" = "#d9534f", "Purchased" = "#5cb85c")) +
  labs(title = "Purchase Distribution",
       subtitle = "Response variable is imbalanced — 78.6% purchased",
       x = NULL, y = "Count") +
  guides(fill = "none") +
  ylim(0, 12000) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )
ggsave("pruchase_distribution.png", path = output_path, 
       width = 11, height = 7, units = "in", dpi = 300)

# INTERPRETATION: The response variable (Purchase) is imbalanced, with the majority
# of patrons purchasing a case of wine. ... 
#
# RECOMMENDATION: 

## Correlation heatmap ##
wine_numeric_clean <- wine_cleaned %>%
  mutate(across(where(is.factor), as.numeric))

cor_mat <- cor(wine_numeric_clean, use = "complete.obs")

png(file.path(output_path, "correlation_heatmap.png"),
    width = 11, height = 9, units = "in", res = 300)
corrplot(cor_mat,
         method = "color",
         type = "upper",
         tl.cex = 0.85,
         tl.col = "black",
         addCoef.col = "black",
         number.cex  = 0.65,
         col = colorRampPalette(c("#d9534f", "white", "#5cb85c"))(200),
         title = "Correlation Matrix — All Variables",
         mar= c(0, 0, 2, 0))
dev.off()

corrplot(cor_mat,
         method = "color",
         type = "upper",
         tl.cex = 0.85,
         tl.col = "black",
         addCoef.col = "black",
         number.cex  = 0.65,
         col = colorRampPalette(c("#d9534f", "white", "#5cb85c"))(200),
         title = "Correlation Matrix — All Variables",
         mar= c(0, 0, 2, 0))

## Correlation bar chart ##
cor_purchase <- cor_mat[, "Purchase"] %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  rename(Correlation = ".") %>%
  filter(Variable != "Purchase") %>%
  mutate(Direction = ifelse(Correlation > 0, "Positive", "Negative"),
         Variable  = fct_reorder(Variable, Correlation))

ggplot(cor_purchase, aes(x = Variable, y = Correlation, fill = Direction)) +
  geom_col(color = "black") +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = round(Correlation, 3),
                hjust = ifelse(Correlation > 0, -0.3, 1.1)),
            size = 4) +
  scale_fill_manual(values = c("Positive" = "#5cb85c", "Negative" = "#d9534f")) +
  coord_flip() +
  labs(title = "Correlations with Purchase",
       subtitle = "Sorted by strength of association",
       x = NULL, y = "Pearson Correlation", fill = NULL) +
  ylim(min(cor_purchase$Correlation) - 0.05,
       max(cor_purchase$Correlation) + 0.05) +
  theme_bw(base_size = 14)
ggsave("correlation_with_purchase.png", path = output_path,
       width = 11, height = 7, dpi = 300)

## Histograms of all continuous variables ##
wine_long <- wine_cleaned %>%
  select(all_of(continuous_vars)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

ggplot(wine_long, aes(x = Value)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white", alpha = 0.85) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(title = "Distribution of Continuous Variables",
       subtitle = "After winsorization",
       x = NULL, y = "Count") + 
  theme_bw()
ggsave("histograms_continuous.png", path = output_path,
       width = 13, height = 12, dpi = 300)

# Pre-model preparation ---------------------------------------------------
# Step 1: Collapse sparse AcidIndex levels
# Go back to wine_cleaned and recode
wine_cleaned <- wine_cleaned %>%
  mutate(AcidIndex = as.numeric(as.character(AcidIndex)),
         AcidIndex_grouped = case_when(
           AcidIndex <= 6  ~ "Low",       # 4, 5, 6
           AcidIndex == 7  ~ "Mid_Low",   # 7
           AcidIndex == 8  ~ "Mid",       # 8
           AcidIndex == 9  ~ "Mid_High",  # 9
           AcidIndex >= 10 ~ "High"       # 10-17 collapsed
         ),
         AcidIndex_grouped = factor(AcidIndex_grouped,
                                    levels = c("Low", "Mid_Low", "Mid", 
                                               "Mid_High", "High")))

cat("AcidIndex grouped distribution:\n")
print(table(wine_cleaned$AcidIndex_grouped))

# Step 2: Rebuild categorical cols with grouped AcidIndex
categorical_cols_model <- c("STARS", "LabelAppeal", "AcidIndex_grouped")

# Step 3: Recreate dummies WITHOUT Cases
wine_model_df <- wine_cleaned %>%
  select(-AcidIndex) %>%
  fastDummies::dummy_columns(
    select_columns = categorical_cols_model,
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  )

cat("\nFinal modeling dataset structure:\n")
str(wine_model_df)
cat("\nDimensions:", dim(wine_model_df), "\n")

# Modeling Framework -----------------------------------------------------
## Set seed for reproducibility ##
set.seed(123)

## Train/test split ##
wine_model_df$u <- runif(nrow(wine_model_df), 0, 1)
train.df <- subset(wine_model_df, u < 0.70) %>% select(-u)
test.df  <- subset(wine_model_df, u >= 0.70) %>% select(-u)

cat("Training set:", nrow(train.df), "| Test set:", nrow(test.df), "\n")

## Drop cases ##
train.clean <- train.df %>% select(-starts_with("Cases"))
test.clean  <- test.df  %>% select(-starts_with("Cases"))

cat("Columns remaining after dropping Cases dummies:\n")
print(names(train.clean))
cat("\nDimensions - Train:", dim(train.clean), "| Test:", dim(test.clean), "\n")

# Check your data split. The sum of the parts should equal the whole ##
dim(wine_model_df)[1]
dim(train.clean)[1]
dim(test.clean)[1]
dim(train.clean)[1] + dim(test.clean)[1]

## Create summary table ##
partition_table <- tibble(
  Dataset = c("Total Sample", "Training Set", "Test Set", "Verification"),
  N = c(
    nrow(wine_model_df),
    nrow(train.clean),
    nrow(test.clean),
    nrow(train.clean) + nrow(test.clean)
  ),
  Percentage = c(
    100.0,
    round(100 * nrow(train.clean) / nrow(wine_model_df), 1),
    round(100 * nrow(test.clean) / nrow(wine_model_df), 1),
    round(100 * (nrow(train.clean) + nrow(test.clean)) / nrow(wine_model_df), 1)
  )
)

print(partition_table, n = Inf)

# Model selection --------------------------------------------------------
## Backwards Selection ##
# Define full model
full_model <- glm(Purchase ~ ., data = train.clean, family = binomial(link = "logit"))
cat("\n=== FULL MODEL ===\n")
summary(full_model)

# Elimination
backward_model <- stepAIC(full_model, direction = "backward", trace = FALSE)

cat("\n=== BACKWARDS ELIMINATION FINAL MODEL ===\n")
summary(backward_model)

## Forward Selection ##
# Define null model
null_model <- glm(Purchase ~ 1, data = train.clean, family = binomial(link = "logit"))
cat("\n=== NULL MODEL ===\n")
summary(null_model)

forward_model <- stepAIC(null_model, 
                         scope = list(lower = null_model, upper = full_model), 
                         direction = "forward", trace = FALSE)

cat("\n=== FORWARDS FINAL MODEL ===\n")
summary(forward_model)

## Stepwise selection ##
stepwise_model <- stepAIC(null_model, 
                          scope = list(lower = null_model, upper = full_model), 
                          direction = "both", trace = FALSE)
summary(stepwise_model)

cat("\n=== STEPWISE ELIMINATION FINAL MODEL ===\n")
summary(stepwise_model)

# Model Coefficients ------------------------------------------------------
library(broom)

coef_table <- tidy(backward_model, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>%
  rename(
    Odds_Ratio = estimate,
    Std_Error = std.error,
    Z_Statistic = statistic,
    P_Value = p.value,
    CI_Lower = conf.low,
    CI_Upper = conf.high
  ) %>%
  arrange(P_Value)

cat("\n=== FINAL MODEL: ODDS RATIOS AND CONFIDENCE INTERVALS ===\n")
print(coef_table, n = Inf)

# Goodness-of-fit ---------------------------------------------------------
# AIC / BIC
cat("AIC:", round(AIC(backward_model), 2), "\n")
cat("BIC:", round(BIC(backward_model), 2), "\n")

# Likelihood ratio test vs null
cat("\nLikelihood Ratio Test (vs null model):\n")
anova(null_model, backward_model, test = "Chisq")

# Model diagnosis and performance on test set -----------------------------
## Predicted probabilities on test set ##
test.clean$pred_prob <- predict(backward_model, newdata = test.clean, type = "response")
test.clean$pred_class <- ifelse(test.clean$pred_prob >= 0.5, 1, 0)

## Confusion matrix ##
cat("\n=== CONFUSION MATRIX (Test Set) ===\n")
conf_matrix <- table(Predicted = test.clean$pred_class, 
                     Actual    = test.clean$Purchase)
print(conf_matrix)

## Performance metrics ##
TN <- conf_matrix[1,1] # True Negative - Negative when actually negative
FP <- conf_matrix[2,1] # False Positive - Positive when actually negative
FN <- conf_matrix[1,2] # False Negative - Negative when actually positive
TP <- conf_matrix[2,2] # True Positive - Positive when actually positive

accuracy <- round((TP + TN) / sum(conf_matrix), 4)
sensitivity <- round(TP / (TP + FN), 4)   # recall / true positive rate
specificity <- round(TN / (TN + FP), 4)   # true negative rate
precision <- round(TP / (TP + FP), 4)
f1_score <- round(2 * precision * sensitivity / (precision + sensitivity), 4)

cat(sprintf("\nAccuracy:    %.1f%%\n", accuracy * 100))
cat(sprintf("Sensitivity: %.1f%%  (%% of actual purchases correctly identified)\n", sensitivity * 100))
cat(sprintf("Specificity: %.1f%%  (%% of actual non-purchases correctly identified)\n", specificity * 100))
cat(sprintf("Precision:   %.1f%%\n", precision * 100))
cat(sprintf("F1 Score:    %.4f\n", f1_score))

## ROC Curve and AUC ##
roc_obj <- pROC::roc(test.clean$Purchase, test.clean$pred_prob)
cat(sprintf("\nAUC: %.4f\n", pROC::auc(roc_obj)))

png(file.path(output_path, "roc_curve.png"),
    width = 11, height = 9, units = "in", res = 300)
plot(roc_obj,
     main = "ROC Curve — Logistic Regression (Test Set)",
     col  = "steelblue", lwd = 2,
     print.auc = TRUE, print.auc.y = 0.4)
dev.off()

plot(roc_obj,
     main = "ROC Curve — Logistic Regression (Test Set)",
     col  = "steelblue", lwd = 2,
     print.auc = TRUE, print.auc.y = 0.4)
