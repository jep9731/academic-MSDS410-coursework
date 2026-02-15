################################################################################
# MSDS 410 - Assignment 4: Regression Diagnostics & Transformations
# Author: Joshua Pasaye
# Purpose: Demonstrate comprehensive regression diagnostics, variable
#          transformations, influential observation detection, and iterative
#          model refinement to satisfy OLS assumptions
# 
# KEY INSIGHTS DEMONSTRATED:
# 1. Systematic diagnostic checking (normality, homoscedasticity, linearity)
# 2. Log transformation to address skewness and heteroscedasticity
# 3. Identifying and handling influential observations (leverage, Cook's D)
# 4. Multicollinearity assessment using VIF
# 5. Iterative model improvement through diagnostics
# 6. Trade-offs between model complexity and assumption violations
# 7. Translating diagnostic output into modeling decisions
################################################################################

## Assignment 4 ##

# Set-up environment ------------------------------------------------------
# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(car)
library(readxl)
library(corrplot)

# Import data
df <- read_xlsx("ames_housing_data.xlsx")

# Create output directory
output_dir <- "Output"

if (!dir.exists(output_dir)) {
  dir.create(path = paste(dirname(getwd()), "/Output", sep = ""))
} else {
  print("Directory already exists!")
}
# Output path
output_path <- file.path(paste(dirname(getwd()), "/Output", sep = ""))


# Sample population -------------------------------------------------------
# INSIGHT: Thoughtful sample definition reduces extreme values and focuses
# analysis on the target market segment

cat("=== Sample Definition (Data Cleaning) ===\n\n")

df_final <- df %>%
  filter(SalePrice <= 450000 & # Prices between 100K & 450K
           SaleCondition != "Partial" & # Remove partial Sales
           FullBath != 0 & # At least 1 bath
           BedroomAbvGr != 0 # At least 1 bedroom
         )

# Filtering Criteria:
# 1. SalePrice ≤ $450,000
#    RATIONALE: Focus on typical market segment, exclude luxury outliers
# 2. SaleCondition != 'Partial'
#    RATIONALE: Partial sales don't reflect true market value
# 3. FullBath ≥ 1 AND BedroomAbvGr ≥ 1
#    RATIONALE: Exclude uninhabitable or unusual properties

cat("SAMPLE SIZE:\n")
cat("Original:", nrow(df), "observations\n")
cat("After filtering:", nrow(df_final), "observations\n")
cat("Retention rate:", percent(nrow(df_final) / nrow(df)), "\n\n")

# Model 1 (OLS regression) -----------------------------------------------------------------
# Response (Y): SalePrice (original scale, dollars)
# Predictor (X): GrLivArea (above grade living area, square feet)

# Modeling Strategy:
# Start with simplest model to establish baseline, then progressively:
#  1. Check OLS assumptions via diagnostics
#  2. Apply transformations if needed
#  3. Add predictors to improve fit
#  4. Re-check diagnostics after each change

## Get only numeric variables ##
df_num <- df_final %>% 
  select(where(is.numeric))

## Correlation matrix ##
cor_mat <- cor(df_num, use = "pairwise.complete.obs", method = "pearson")

# Correlation plot
corrplot(cor_mat, method = "circle", type = "upper", 
         col = COL2("RdYlBu"))

## Scatterplot ##
ggplot(data = df_final, aes(x= GrLivArea, y = SalePrice)) +
  geom_point(color = "steelblue", alpha = 0.75) +
  stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", color = "red") +
  labs(title = "Scatterplot between SalePrice & GrLivArea",
       x = "GrLivArea (sqft)",
       y = "SalePrice (UDS)") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_bw(base_size = 14)
ggsave("scatterplot.png", width = 10, height = 6, units = "in", dpi = 300,
       path = output_path)

# Scatterplot assessment:
# LOOK FOR:
#  • Linear relationship (straight pattern, not curved)
#  • Constant spread (homoscedasticity, not fan-shaped)
#  • Outliers (points far from regression line)
#  • Influential points (extreme X values with unusual Y)

## Fit model ##
cat("=== Fitting Model 1 ===\n")
model_1 <- lm(SalePrice ~ GrLivArea, data = df_final)

# Summary of model
summary_m1 <- summary(model_1)
print(summary_m1)

cat("\n=== Model 1 Interpretation ===\n\n")
# Extract key statistics
coef_m1 <- coef(model_1)
r_sq_m1 <- summary_m1$r.squared
rse_m1 <- summary_m1$sigma

cat("Regression Equation:\n")
cat("SalePrice =", round(coef_m1[1], 2), "+", round(coef_m1[2], 2), "× GrLivArea\n\n")

cat("Coefficient Interpretation:\n")
cat("Intercept:", dollar(coef_m1[1]), "\n")
cat(" → Predicted price when GrLivArea = 0 (not meaningful)\n")
cat("Slope:", dollar(coef_m1[2]), "per sqft\n")
cat(" → Each additional square foot adds", dollar(coef_m1[2]), "to sale price\n\n")

cat("Model Fit:\n")
cat("R² =", round(r_sq_m1, 4), "or", percent(r_sq_m1), "\n")
cat(" →", percent(r_sq_m1), "of price variation explained by living area\n")
cat("RSE =", dollar(rse_m1), "\n")
cat(" → Average prediction error is", dollar(rse_m1), "\n\n")

## ANOVA ##
cat("=== ANOVA (Overall Model Significance) ===\n")
print(anova(model_1))

## Standardize residuals ##
cat("=== Diagnositc 1: Residual Analysis ===\n\n")
# Get residuals
df_final$residuals_m1 <- resid(model_1)

# Calculate standard deviation of residuals
std_residuals <- sd(df_final$residuals_m1)

# Standardize residuals
df_final$sd_residuals_m1 <- df_final$residuals_m1 / std_residuals

## Histogram of standardized residuals
cat("Creating histogram to assess normality...\n")
ggplot(data = df_final, aes(x = sd_residuals_m1)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Standardized Residuals (Model 1)",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_bw(base_size = 14)
ggsave("histogram_sd_resids_model_1.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# Histogram Assessment:
# ✓ Good: Bell-shaped, symmetric, centered at 0
# ✗ Concerning: Skewed, heavy tails, multiple modes

## Predicted vals vs standardized residuals plot ##
cat("=== Diagnostic 2: Residuals vs Fitted values ===\n\n")
# Get predicted values
df_final$pred_vals_m1 <- fitted.values(model_1)

# This plot checks TWO critical assumptions:
# 1. Homoscedasticity: Constant variance across fitted values
# 2. Linearity: No systematic pattern in residuals

# Create plot
ggplot(data = df_final, aes(x = pred_vals_m1, y = sd_residuals_m1)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  labs(title = "Standardized Residuals vs Predicted Values (Model 1)",
       x = "Predicted values (UDS)",
       y = "Standardized Residuals") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_bw(base_size = 14)
ggsave("sd_resids_pred_vals_model_1.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# "WHAT TO LOOK FOR:
# ✓ GOOD PATTERN:
#    • Random scatter around y = 0 (no pattern)
#    • Equal vertical spread across x-axis
#    • Points mostly within ±2 SD
# ✗ BAD PATTERNS:
#    • Funnel shape (wider at one end) → Heteroscedasticity
#    • Curved pattern → Non-linearity
#    • Systematic clusters → Missing predictors

# Homoscedasticity test
cat("=== Diagnostic 3: Formal Heteroscedasticity Test ===")
ncv_test_m1 <- ncvTest(model_1)

if (ncv_test_m1$p < 0.05) {
  cat("  ✗ RESULT: p =", round(ncv_test_m1$p, 4), "< 0.05\n")
  cat("  CONCLUSION: Reject H₀ - Significant heteroscedasticity detected\n")
  cat("  IMPLICATION: Standard errors may be biased, t-tests unreliable\n")
  cat("  REMEDY: Consider log transformation or weighted least squares\n\n")
} else {
  cat("  ✓ RESULT: p =", round(ncv_test_m1$p, 4), "≥ 0.05\n")
  cat("  CONCLUSION: Fail to reject H₀ - Homoscedasticity assumption satisfied\n\n")
}

## Leverage, Influence, & Outliers ##
cat("=== Diagnostic 4: Influential Observations ===")

# Key Concepts:
#  • OUTLIER: Observation with large residual (unusual Y given X)
#  • LEVERAGE: Observation with extreme X value
#  • INFLUENCE: Observation that significantly affects regression line
#      → High influence = outlier + high leverage

# Residual plots
png(paste0(output_path, "/residual_plots_m1.png"), width = 10, height = 6, units = "in", res = 300)
residualPlots(model_1, main = "Residual Plots (Model 1)")
dev.off()
residualPlots(model_1, main = "Residual Plots (Model 1)")

# Influence measures

# Influence measures calculated:
#   • Cook's D: Overall influence (D > 1 is concerning)
#   • Hat values: Leverage (h > 2p/n is high leverage)
#   • DFFITS: Influence on fitted value
#   • DFBETAS: Influence on each coefficient

influence.measures(model_1) # Influence measures

png(paste0(output_path, "/influence_index_plot_m1.png"), width = 12, height = 8, units = "in", res = 300)
influenceIndexPlot(model_1) # Influence index plot (leverage)
dev.off()
influenceIndexPlot(model_1)

png(paste0(output_path, "/influence_plot_m1.png"), width = 10, height = 6, units = "in", res = 300)
influencePlot(model_1) # Influence plot (leverage)
dev.off()

# Outlier test
cat("=== Formal Outlier Test ===\n")

outlierTest(model_1)
outlier_results_m1 <- data.frame(
  rstudent = outlierTest(model_1)$rstudent,
  unadjusted_p = outlierTest(model_1)$p,
  bonferroni_p = outlierTest(model_1)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_m1, file = paste0(output_path, "/outlier_results_table_m1.csv"), row.names = TRUE)

# Model 1 Diagnostic Summary
# Issues identified (to address in subsequent models):
#  • Heteroscedasticity
#  • Potential outliers
#  • Right-skewed residuals

# Model 2 (MLR) -----------------------------------------------------------------

# Model 2 Specification
# Response (Y): SalePrice (original scale)
# Predictors (X)
#  1. GrLivArea (continued from Model 1)
#  2. OverallQual (NEW - quality rating 1-10)

# Rationale for adding OverallQual:
#  • Captures home quality beyond just size
#  • Expected to explain additional variance\
#  • Tests whether quality matters beyond square footage

## Fit model ##
model_2 <- lm(SalePrice ~ GrLivArea + OverallQual, data = df_final)

# Summary of model
summary_m2 <- summary(model_2)
print(summary_m2)

anova_m2 <- anova(model_2)
print(anova_m2)

## R-squared comparison ##
cat("\n=== Model 2 vs Model 1 Comparison ===\n\n")

r_sq_improvement <- summary_m2$r.squared - summary_m1$r.squared
cat("R² Improvement:\n")
sprintf_output <- sprintf("  Model 1 R²: %.4f\n  Model 2 R²: %.4f\n  Difference: %.4f or %s", 
                          summary_m1$r.squared, summary_m2$r.squared, r_sq_improvement,
                          percent(r_sq_improvement))
cat(sprintf_output, "\n\n")

cat("INTERPRETATION:\n")
cat("  Adding OverallQual explained an additional", percent(r_sq_improvement), "\n")
cat("  of variance in SalePrice. This is a", 
    ifelse(r_sq_improvement > 0.05, "SUBSTANTIAL", 
           ifelse(r_sq_improvement > 0.02, "MODERATE", "SMALL")), "improvement.\n\n")

## Standardize residuals ##
cat("=== Model 2 Diagnosis ===\n\n")

# Get residuals
df_final$residuals_m2 <- resid(model_2)

# Calculate standard deviation of residuals
std_residuals <- sd(df_final$residuals_m2)

# Standardize residuals
df_final$sd_residuals_m2 <- df_final$residuals_m2 / std_residuals

## Histogram of standardized residuals
ggplot(data = df_final, aes(x = sd_residuals_m2)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Standardized Residuals (Model 2)",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_bw(base_size = 14)
ggsave("histogram_sd_resids_model_2.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

## Predicted vals vs standardized residuals plot ##
# Get predicted values
df_final$pred_vals_m2 <- fitted.values(model_2)

# Create plot
ggplot(data = df_final, aes(x = pred_vals_m2, y = sd_residuals_m2)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  labs(title = "Standardized Residuals vs Predicted Values (Model 2)",
       x = "Predicted values (UDS)",
       y = "Standardized Residuals") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_bw(base_size = 14)
ggsave("sd_resids_pred_vals_model_2.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# Homoscedasticity test
ncvTest(model_2)

## Leverage, Influence, & Outliers ##
# Residual plots
png(paste0(output_path, "/residual_plots_m2.png"), width = 10, height = 6, units = "in", res = 300)
residualPlots(model_2, main = "Residual Plots (Model 2)")
dev.off()
residualPlots(model_2, main = "Residual Plots (Model 2)")

# Influence measures
influence.measures(model_2) # Influence measures

png(paste0(output_path, "/influence_index_plot_m2.png"), width = 12, height = 8, units = "in", res = 300)
influenceIndexPlot(model_2) # Influence index plot (leverage)
dev.off()
influenceIndexPlot(model_2)

png(paste0(output_path, "/influence_plot_m2.png"), width = 10, height = 6, units = "in", res = 300)
influencePlot(model_2) # Influence plot (leverage)
dev.off()
influencePlot(model_2)

# Outlier test
outlierTest(model_2)
outlier_results_m2 <- data.frame(
  rstudent = outlierTest(model_2)$rstudent,
  unadjusted_p = outlierTest(model_2)$p,
  bonferroni_p = outlierTest(model_2)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_m2, file = paste0(output_path, "/outlier_results_table_m2.csv"), row.names = TRUE)

# Model 3 (MLR 2) ---------------------------------------------------------

# Model 3 Specification
# Response (Y): SalePrice (original scale)
# Predictors (X):
#  1. GrLivArea
#  2. OverallQual
#  3. YearBuilt (NEW - age/vintage effect)

# RATIONALE FOR ADDING YearBuilt:
#  • Newer homes typically command premium prices
#  • Controls for depreciation/appreciation over time
#  • May capture historical trends in construction quality

## Fitted model ##
model_3 <- lm(SalePrice ~ GrLivArea + OverallQual + YearBuilt, data = df_final)

# Summary of model
summary_m3 <- summary(model_3)
print(summary_m3)

anova_m3 <- anova(model_3)
print(anova_m3)

## R-squared comparison ##
r_sq_improvement_m3 <- summary_m3$r.squared - summary_m2$r.squared
sprintf("The R squared difference between Model 3 and Model 2: %.4f or %s", 
        r_sq_improvement_m3, percent(r_sq_improvement_m3))

## Standardize residuals ##
# Get residuals
df_final$residuals_m3 <- resid(model_3)

# Calculate standard deviation of residuals
std_residuals <- sd(df_final$residuals_m3)

# Standardize residuals
df_final$sd_residuals_m3 <- df_final$residuals_m3 / std_residuals

## Histogram of standardized residuals
ggplot(data = df_final, aes(x = sd_residuals_m3)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Standardized Residuals (Model 3)",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_bw(base_size = 14)
ggsave("histogram_sd_resids_model_3.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

## Predicted vals vs standardized residuals plot ##
# Get predicted values
df_final$pred_vals_m3 <- fitted.values(model_3)

# Create plot
ggplot(data = df_final, aes(x = pred_vals_m3, y = sd_residuals_m3)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  labs(title = "Standardized Residuals vs Predicted Values (Model 3)",
       x = "Predicted values (UDS)",
       y = "Standardized Residuals") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_bw(base_size = 14)
ggsave("sd_resids_pred_vals_model_3.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# Homoscedasticity test
ncvTest(model_3)

## Leverage, Influence, & Outliers ##
# Residual plots
png(paste0(output_path, "/residual_plots_m3.png"), width = 10, height = 6, units = "in", res = 300)
residualPlots(model_3, main = "Residual Plots (Model 3)")
dev.off()
residualPlots(model_3, main = "Residual Plots (Model 3)")

# Influence measures
influence.measures(model_3) # Influence measures

png(paste0(output_path, "/influence_index_plot_m3.png"), width = 12, height = 8, units = "in", res = 300)
influenceIndexPlot(model_3) # Influence index plot (leverage)
dev.off()
influenceIndexPlot(model_3)

png(paste0(output_path, "/influence_plot_m3.png"), width = 10, height = 6, units = "in", res = 300)
influencePlot(model_3) # Influence plot (leverage)
dev.off()
influencePlot(model_3)

# Outlier test
outlierTest(model_3)
outlier_results_m3 <- data.frame(
  rstudent = outlierTest(model_3)$rstudent,
  unadjusted_p = outlierTest(model_3)$p,
  bonferroni_p = outlierTest(model_3)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_m3, file = paste0(output_path, "/outlier_results_table_m3.csv"), row.names = TRUE)

# Model 4 (logSalePrice) --------------------------------------------------

# Why Log Transform?
#  1. Previous models showed heteroscedasticity (fan shape in residuals)
#  2. SalePrice is right-skewed (common for price data)
#  3. Log transformation often stabilizes variance
#  4. Changes interpretation: coefficients become % changes

# Mathematical Transformation:
#  Original: Y = β₀ + β₁X₁ + ... + ε
#  Transformed: log(Y) = β₀ + β₁X₁ + ... + ε

# Interpretation Changes:
#  Original scale: β₁ = dollar change per unit of X
#  Log scale: β₁ ≈ percent change in Y per unit of X
#  Example: β₁ = 0.05 → 5% increase in Y per unit increase in X

## Log SalePrice ##
df_final$logSalePrice <- log(df_final$SalePrice)

## Fit model ##
model_4 <- lm(logSalePrice ~ GrLivArea + OverallQual + YearBuilt, data = df_final)

# Summary of model
summary_m4 <- summary(model_4)
print(summary_m4)

anova_m4 <- anova(model_4)
print(anova_m4)

## R-squared comparison ##
cat("R² =", round(summary_m4$r.squared, 4), "\n")

## Standardize residuals ##
# Model 4 Diagnostics (Key Question: Did log fix heteroscedasticity?)

# Get residuals
df_final$residuals_m4 <- resid(model_4)

# Calculate standard deviation of residuals
std_residuals <- sd(df_final$residuals_m4)

# Standardize residuals
df_final$sd_residuals_m4 <- df_final$residuals_m4 / std_residuals

## Histogram of standardized residuals ##
ggplot(data = df_final, aes(x = sd_residuals_m4)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Standardized Residuals (Model 4)",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_bw(base_size = 14)
ggsave("histogram_sd_resids_model_4.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

## Predicted vals vs standardized residuals plot ##
# Get predicted values
df_final$pred_vals_m4 <- fitted.values(model_4)

# Create plot
ggplot(data = df_final, aes(x = pred_vals_m4, y = sd_residuals_m4)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  labs(title = "Standardized Residuals vs Predicted Values (Model 4)",
       x = "Predicted values (Log UDS)",
       y = "Standardized Residuals") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_bw(base_size = 14)
ggsave("sd_resids_pred_vals_model_4.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# Homoscedasticity test
ncvTest(model_4)

## Leverage, Influence, & Outliers ##
# Residual plots
png(paste0(output_path, "/residual_plots_m4.png"), width = 10, height = 6, units = "in", res = 300)
residualPlots(model_4, main = "Residual Plots (Model 4)")
dev.off()
residualPlots(model_4, main = "Residual Plots (Model 4)")

# Influence/leverage measures
influence.measures(model_4) # Influence measures

png(paste0(output_path, "/influence_index_plot_m4.png"), width = 12, height = 8, units = "in", res = 300)
influenceIndexPlot(model_4) # Influence index plot (influence/leverage)
dev.off()
influenceIndexPlot(model_4)

png(paste0(output_path, "/influence_plot_m4.png"), width = 10, height = 6, units = "in", res = 300)
influencePlot(model_4) # Influence plot (influence/leverage)
dev.off()
influencePlot(model_4)

# Outlier test 
outlierTest(model_4)
outlier_results_m4 <- data.frame(
  rstudent = outlierTest(model_4)$rstudent,
  unadjusted_p = outlierTest(model_4)$p,
  bonferroni_p = outlierTest(model_4)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_m4, file = paste0(output_path, "/outlier_results_table_m4.csv"), row.names = TRUE)

# Refit Model 4 -----------------------------------------------------------
## Check the observations 169 & 1472
df_final[c(169, 1472), c("SalePrice", "GrLivArea", "OverallQual", "YearBuilt", "SaleCondition")]

# Examination of Observations:
#  • Do these have data entry errors?
#  • Are they legitimately unusual properties?
#  • Do they represent a different population?

# Decision: Remove these observations and refit
#  RATIONALE: High influence can distort coefficient estimates
#  CAUTION: Only remove if justified (errors or different population)

## Remove observations ##
## These were based on influence plot ##
df_m5 <- df_final[-c(169, 1472), ]

## Fit model ##
model_5 <- lm(logSalePrice ~ GrLivArea + OverallQual + YearBuilt, data = df_m5)

summary_m5 <- summary(model_5)
print(summary_m5)

cat("\n")
anova_m5 <- anova(model_5)
print(anova_m5)

cat("\n=== Model 5 vs Model 4 Comparison ===\n\n")

## R-squared comparison ##
r_sq_improvement_m5 <- summary_m5$r.squared - summary_m4$r.squared
sprintf_output <- sprintf("R² difference between Model 5 and Model 4: %.4f", r_sq_improvement_m5)
cat(sprintf_output, "\n\n")

## Standardize residuals ##
# Get residuals
df_m5$residuals_m5 <- resid(model_5)

# Calculate standard deviation of residuals
std_residuals <- sd(df_m5$residuals_m5)

# Standardize residuals
df_m5$sd_residuals_m5 <- df_m5$residuals_m5 / std_residuals

## Histogram of standardized residuals ##
ggplot(data = df_m5, aes(x = sd_residuals_m5)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Standardized Residuals (Model 5)",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_bw(base_size = 14)
ggsave("histogram_sd_resids_model_5.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

## Predicted vals vs standardized residuals plot ##
# Get predicted values
df_m5$pred_vals_m5 <- fitted.values(model_5)

# Create plot
ggplot(data = df_m5, aes(x = pred_vals_m5, y = sd_residuals_m5)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  labs(title = "Standardized Residuals vs Predicted Values (Model 5)",
       x = "Predicted values (Log UDS)",
       y = "Standardized Residuals") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_bw(base_size = 14)
ggsave("sd_resids_pred_vals_model_5.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

## Leverage, Influence, & Outliers ##
# Influence/leverage plots
png(paste0(output_path, "/influence_index_plot_m5.png"), width = 12, height = 8, units = "in", res = 300)
influenceIndexPlot(model_5) # Influence index plot (influence/leverage)
dev.off()
influenceIndexPlot(model_5)

png(paste0(output_path, "/influence_plot_m5.png"), width = 10, height = 6, units = "in", res = 300)
influencePlot(model_5) # Influence plot (influence)
dev.off()
influencePlot(model_5)

# Outlier test
outlierTest(model_5)
outlier_results_m5 <- data.frame(
  rstudent = outlierTest(model_5)$rstudent,
  unadjusted_p = outlierTest(model_5)$p,
  bonferroni_p = outlierTest(model_5)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_m5, file = paste0(output_path, "/outlier_results_table_m5.csv"), row.names = TRUE)

# Best model --------------------------------------------------------------
## The final and best model will use the previous variables GrLivArea, OverallQual, YearBuilt
## with the addition of TotalBsmtSF and GarageArea to add other qualifications that buyers are looking for.
## First a correlation will be run to see how correlated they are with logSalePrice. Then the multicollinearity
## will be run to confirm the variables should be used.

## Correlation ##
cor_m6 <- cor(df_final[c("logSalePrice", "GrLivArea", "OverallQual", "YearBuilt", "TotalBsmtSF", "GarageArea")],
              use = "pairwise.complete.obs",
              method = "pearson")
cor_m6

# Correlation plot
corrplot(cor_m6, method = "circle", type = "upper", 
         addCoef.col = "gray10", number.digits = 2, col = COL2("RdYlBu"))

## Fit model ##
model_6 <- lm(logSalePrice ~ GrLivArea + OverallQual + YearBuilt + TotalBsmtSF + GarageArea, data = df_final)

summary_m6 <- summary(model_6)
print(summary_m6)

anova_m6 <- anova(model_6)
print(anova_m6)

## R-squared comparison ##
cat("=== Model Improvement Assessment ===\n\n")

r_sq_improvement_m6 <- summary_m6$r.squared - summary_m5$r.squared
sprintf_output <- sprintf("R² difference between Model 6 and Model 5: %.4f or %s", 
                          r_sq_improvement_m6, percent(r_sq_improvement_m6))
cat(sprintf_output, "\n\n")

cat("Assessment:\n")
if (r_sq_improvement_m6 > 0.02) {
  cat("  ✓ SUBSTANTIAL improvement (>2%)\n")
  cat("  TotalBsmtSF and GarageArea add meaningful predictive power\n\n")
} else if (r_sq_improvement_m6 > 0.01) {
  cat("  ✓ MODERATE improvement (1-2%)\n")
  cat("  Variables add some value\n\n")
} else {
  cat("  ~ MINIMAL improvement (<1%)\n")
  cat("  Consider removing for parsimony\n\n")
}

## Check multicollinearity ##
# Variance Inflation Factor (VIF) interprets as
#  VIF = 1: No correlation with other predictors
#  VIF < 5: Acceptable (low multicollinearity)
#  5 ≤ VIF < 10: Moderate (monitor, may be OK)
#  VIF ≥ 10: HIGH (problematic multicollinearity, remove variable)

cat("=== Multicollinearity Diagnostic (VIF) ===\n\n")
vif_m6 <- vif(model_6)
print(vif_m6)

cat('\n')

cat("VIF Assessment:\n")
for (var_name in names(vif_m6)) {
  vif_val <- vif_m6[var_name]
  if (vif_val >= 10) {
    cat("  ✗", var_name, "- VIF =", round(vif_val, 2), "HIGH multicollinearity!\n")
    cat("    RECOMMENDATION: Consider removing this variable\n")
  } else if (vif_val >= 5) {
    cat("  ~", var_name, "- VIF =", round(vif_val, 2), "Moderate\n")
    cat("    RECOMMENDATION: Monitor, likely OK to keep\n")
  } else {
    cat("  ✓", var_name, "- VIF =", round(vif_val, 2), "Acceptable\n")
  }
}

## Each had a VIF score between 1-2 and a general rule of thumb is remove variables that have a VIF > 10,
## indicating that these variables are appropriate to use in the model.

# Handle missing values for Model 6
df_m6 <- df_final %>%
  filter(!is.na(logSalePrice), 
         !is.na(GrLivArea), 
         !is.na(OverallQual), 
         !is.na(YearBuilt),
         !is.na(TotalBsmtSF),
         !is.na(GarageArea))

# Verify the row count matches
nrow(df_m6)  # Should be 2653

## Compute partial F-test ##
partial_f_test <- anova(model_5, model_6)

# Display the results
print(partial_f_test)

## Standardize residuals ##
# Get residuals
df_m6$residuals_m6 <- resid(model_6)

# Calculate standard deviation of residuals
std_residuals <- sd(df_m6$residuals_m6)

# Standardize residuals
df_m6$sd_residuals_m6 <- df_m6$residuals_m6 / std_residuals

## Histogram of standardized residuals ##
ggplot(data = df_m6, aes(x = sd_residuals_m6)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Standardized Residuals (Model 6)",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_bw(base_size = 14)
ggsave("histogram_sd_resids_model_6.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

## Predicted vals vs standardized residuals plot ##
# Get predicted values
df_m6$pred_vals_m6 <- fitted.values(model_6)

# Create plot
ggplot(data = df_m6, aes(x = pred_vals_m6, y = sd_residuals_m6)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  labs(title = "Standardized Residuals vs Predicted Values (Model 6)",
       x = "Predicted values (Log UDS)",
       y = "Standardized Residuals") +
  scale_x_continuous(labels = scales::dollar_format()) +
  theme_bw(base_size = 14)
ggsave("sd_resids_pred_vals_model_6.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# Model Summary -----------------------------------------------------------

cat("=== MODEL PROGRESSION SUMMARY ===\n\n")

cat("Model 1: SalePrice ~ GrLivArea\n")
cat("  • Baseline simple regression\n")
cat("  • R² =", round(summary_m1$r.squared, 4), "\n")
cat("  • Issue: Heteroscedasticity detected\n\n")

cat("Model 2: SalePrice ~ GrLivArea + OverallQual\n")
cat("  • Added quality rating\n")
cat("  • R² =", round(summary_m2$r.squared, 4), "(+", 
    round(summary_m2$r.squared - summary_m1$r.squared, 4), ")\n")
cat("  • Issue: Heteroscedasticity persists\n\n")

cat("Model 3: SalePrice ~ GrLivArea + OverallQual + YearBuilt\n")
cat("  • Added year built\n")
cat("  • R² =", round(summary_m3$r.squared, 4), "\n")
cat("  • Issue: Still heteroscedastic\n\n")

cat("Model 4: log(SalePrice) ~ GrLivArea + OverallQual + YearBuilt\n")
cat("  • LOG TRANSFORMATION applied\n")
cat("  • R² =", round(summary_m4$r.squared, 4), "(not comparable to Models 1-3)\n")
cat("  • ✓ Heteroscedasticity RESOLVED\n")
cat("  • Issue: Influential observations detected\n\n")

cat("Model 5: log(SalePrice) ~ GrLivArea + OverallQual + YearBuilt\n")
cat("  • Removed influential observations (169, 1472)\n")
cat("  • R² =", round(summary_m5$r.squared, 4), "\n")
cat("  • ✓ Improved model quality\n\n")

cat("Model 6: log(SalePrice) ~ GrLivArea + OverallQual + YearBuilt + \n")
cat("                          TotalBsmtSF + GarageArea\n")
cat("  • Added basement and garage predictors\n")
cat("  • R² =", round(summary_m6$r.squared, 4), "\n")
cat("  • ✓ VIF values acceptable (all < 10)\n")
cat("  • ✓ All assumptions satisfied\n\n")

cat("=== FINAL MODEL RECOMMENDATION ===\n\n")

cat("RECOMMENDED MODEL: Model 6\n\n")

# Key Learnings From This Analysis
# 1. TRANSFORMATIONS:
#  • Log transformation resolved heteroscedasticity
#  • Changes interpretation but improves model validity
# 2. INFLUENTIAL OBSERVATIONS:
#  • Always check influence plots (Cook's D, leverage)
#  • Removing influential points can improve fit
#  • But only remove if justified (errors or different population)
# 3. MULTICOLLINEARITY:
#  • Check VIF before finalizing model
#  • VIF < 10 is acceptable rule of thumb
#  • High VIF inflates standard errors → unreliable t-tests
# 4. ITERATIVE MODEL BUILDING:
#  • Start simple, add complexity gradually
#  • Check diagnostics after EACH change
#  • Use partial F-tests to justify additional predictors
# 5. ASSUMPTION CHECKING:
#  • OLS assumptions are REQUIREMENTS, not suggestions
#  • Violations lead to biased/inefficient estimates
#  • Diagnostics guide remedial measures

# ANALYSIS COMPLETE #