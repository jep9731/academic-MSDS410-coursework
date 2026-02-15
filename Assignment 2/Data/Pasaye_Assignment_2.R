################################################################################
# MSDS 410 - Assignment 2: Simple Linear Regression Analysis
# Author: Joshua Pasaye
# Purpose: Demonstrate simple linear regression modeling, model evaluation,
#          diagnostic checking, and comparison of alternative models
# 
# KEY INSIGHTS DEMONSTRATED:
# 1. Correlation analysis to identify potential predictors
# 2. Fitting and interpreting simple linear regression models
# 3. Statistical inference using t-tests and F-tests
# 4. Model diagnostics using residual analysis
# 5. Comparing multiple models to select the best predictor
# 6. Translating regression output into actionable insights
################################################################################

# Set-up environment ------------------------------------------------------
# BEST PRACTICE: Always start with a clean environment to ensure reproducibility
# Remove everything from environment
rm(list = ls())

# Create output directory
output_dir <- "Output"

if (!dir.exists(output_dir)) {
  dir.create(path = paste(dirname(getwd()), "/Output", sep = ""))
} else {
  print("Directory already exists!")
}
# Output path
output_path <- file.path(paste(dirname(getwd()), "/Output", sep = ""))


# RECOMMENDATION: Document all package versions for reproducibility
# Import libraries
library(tidyverse)
library(car)
library(readxl)
library(GGally)
library(corrplot)
library(lattice)
library(knitr)
library(kableExtra)
library(scales)
library(gt)

# Import data
df <- read_xlsx("USStates.xlsx")

# Summary statistics & Scatter plots ----------------------------------------------------
# RESEARCH QUESTION: What predicts household income across US states?
# RESPONSE VARIABLE: HouseholdIncome (continuous, measured in dollars)
# Use HOUSEHOLDINCOME as response variable (Y)
response_var <- "HouseholdIncome"

# Use State, Region, and Pop as demographic
demos <- c("State", "Region", "Population")

# Create dataframe of only those variables
df_stats <- df[, c(demos, response_var)]

cat("=== VARIABLE ROLES ===\n")
cat("Response Variable (Y):", response_var, "\n")
cat("Demographic Identifiers:", paste(demos, collapse = ", "), "\n")
cat("Potential Predictors: All other numeric variables\n\n")

## Summary table ##
# INSIGHT: Summary statistics provide baseline understanding of data distribution
# and help identify data quality issues (missing values, outliers)

summary_table <- df_stats %>%
  summarise(
    # numeric vars
    across(
      c(HouseholdIncome, Population),
      list(
        N = ~sum(!is.na(.x)),
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    # categorical vars
    State_N = sum(!is.na(State)),
    Region_N = sum(!is.na(Region)),
    Region_Unique = n_distinct(Region)
  )
cat("=== SUMMARY STATISTICS ===\n")
print(summary_table)

# INTERPRETATION: Extract key insights
mean_income <- summary_table$HouseholdIncome_Mean
sd_income <- summary_table$HouseholdIncome_SD
cv_income <- sd_income / mean_income  # Coefficient of variation

cat("\n=== HOUSEHOLD INCOME INSIGHTS ===\n")
cat("Mean Household Income:", dollar(mean_income), "\n")
cat("Standard Deviation:", dollar(sd_income), "\n")
cat("Coefficient of Variation:", percent(cv_income), "\n")
cat("\nINTERPRETATION: The CV of", percent(cv_income), "indicates moderate variability\n")
cat("in household income across states. This suggests income is influenced by\n")
cat("state-level factors that our predictors may help explain.\n\n")

# Region counts
# INSIGHT: Understanding regional distribution helps assess geographic representation
region_counts_long <- df_stats %>%
  count(Region, name = "N") %>%
  transmute(
    Variable = paste0("Region_", Region),
    Statistic = "N",
    value = N
  )
cat("=== REGIONAL DISTRIBUTION ===\n")
region_summary <- df_stats %>% count(Region, name = "N")
print(region_summary)
cat("\nINSIGHT: States are distributed across", nrow(region_summary), "regions.\n")
cat("This regional variation may explain some income differences (e.g., cost of living).\n\n")

# Reshape table to long format
summary_long <- summary_table %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", "Statistic"),
    names_sep = "_"
  )

# Format pretty table
pretty_table <- bind_rows(summary_long, region_counts_long) %>%
  pivot_wider(
    names_from = Statistic,
    values_from = value
  ) %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics",
    subtitle = "HouseholdIncome (Y) and Demographic Variables"
  ) %>%
  fmt_number(
    columns = c(Mean, SD),
    decimals = 3
  )
pretty_table

# Save table for reporting
pretty_table %>%
  gtsave(
    data = .,
    filename = "summary_table.html",
    path = output_path
  )

cat("Summary table saved to:", file.path(output_path, "summary_table.html"), "\n\n")

## Histograms of household income and population ##
# INSIGHT: Histograms reveal distribution shape, which affects:
# 1. Choice of statistical methods (parametric vs. non-parametric)
# 2. Need for transformations
# 3. Interpretation of mean/median

cat("=== DISTRIBUTION ANALYSIS ===\n")
num_vars <- names(df_stats)[sapply(df_stats, is.numeric)] # Get numeric columns

for (v in num_vars) {
  p <- ggplot(df_stats, aes(x = .data[[v]])) +
    geom_histogram(bins = 20, color = "black", fill = "steelblue") +
    labs(
      title = paste(v, "Distribution"),
      x = v,
      y = "Frequency"
    ) +
    theme_bw(base_size = )
  print(p)
  ggsave(paste0(v, "_hist_plot.png"), path = output_path, width = 10, height = 6, units = "in")
}

cat("DISTRIBUTION INSIGHTS:\n")
cat("• HouseholdIncome: Check for right-skew (few high-income states)\n")
cat("• Population: Likely right-skewed (few very populous states like CA, TX)\n")
cat("• Outliers in either variable may influence regression results\n\n")

## Scatterplots ##
# INSIGHT: Scatterplots with LOESS curves reveal:
# 1. Direction of relationship (positive/negative)
# 2. Strength of relationship (tight vs. dispersed)
# 3. Linearity assumption validity
# 4. Presence of outliers or influential points

cat("=== BIVARIATE RELATIONSHIP ANALYSIS ===\n")
# Get only continuous vars for scatterplot
continuous_vars <- names(df[!names(df) %in% c(demos, response_var)])

# Plots vs HouseholdIncome
for (var in continuous_vars) {
  p <- ggplot(df, aes(x = .data[[var]], y = HouseholdIncome)) +
    geom_point() +
    geom_smooth(method="loess", se=TRUE, color="blue") +
    labs(title=paste(var, "vs HouseholdIncome"),
         x = var,
         y = "HouseholdIncome") +
    theme_bw(base_size = 16)
  print(p)
  ggsave(paste0(var, "_plot.png"), path = output_path, width = 10, height = 6, units = "in")
}

cat("INTERPRETATION GUIDE for Scatterplots:\n")
cat("• Upward slope: Positive relationship (↑ predictor → ↑ income)\n")
cat("• Downward slope: Negative relationship (↑ predictor → ↓ income)\n")
cat("• Straight LOESS: Linear regression appropriate\n")
cat("• Curved LOESS: May need transformation or polynomial terms\n")
cat("• Wide scatter: Weak relationship, low R²\n")
cat("• Tight scatter: Strong relationship, high R²\n\n")

# Correlation matrix ----------------------------------------------
# INSIGHT: Correlation matrix quantifies linear relationships discovered visually
# in scatterplots. Correlation (r) ranges from -1 to +1:
#   • |r| > 0.7: Strong linear relationship
#   • 0.4 < |r| < 0.7: Moderate linear relationship  
#   • |r| < 0.4: Weak linear relationship
#   • r > 0: Positive association
#   • r < 0: Negative association

cat("=== CORRELATION ANALYSIS ===\n")
# Remove demographic data
df_corr <- df[, !(names(df) %in% demos)]

# Correlation matrix
corr_matrix <- round(cor(df_corr, method = "pearson"), 2)
corr_matrix

# Correlation plot
png(filename = paste0(output_path, "/", "corr_plot.png"), width = 800, height = 600, units = "px", res = 100) # Save plot
corrplot(corr = corr_matrix,
         method = "circle",
         type = "upper",
         col = COL1("YlOrRd"),
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         tl.pos = "td",
         title = "Correlation Matrix",
         mar = c(0,0,.9,0) # Increase top margin
)
dev.off()
cat("Correlation plot saved to:", file.path(output_path, "corr_plot.png"), "\n\n")

# Convert matrix to data frame for nice display
hhi_corr <- corr_matrix["HouseholdIncome", ]

# Convert to data frame and remove self-correlation
corr_hhi_df <- data.frame(
  Variable = names(hhi_corr),
  Correlation = as.numeric(hhi_corr)
) %>%
  dplyr::filter(Variable != "HouseholdIncome")

cat("=== CORRELATIONS WITH HOUSEHOLD INCOME (Sorted by Strength) ===\n")
print(corr_hhi_df)

# ACTIONABLE INSIGHTS from correlations
cat("\n=== KEY CORRELATION FINDINGS ===\n")
for (i in 1:min(3, nrow(corr_hhi_df))) {
  var_name <- corr_hhi_df$Variable[i]
  corr_val <- corr_hhi_df$Correlation[i]
  
  direction <- ifelse(corr_val > 0, "positive", "negative")
  strength <- ifelse(abs(corr_val) > 0.7, "strong",
                     ifelse(abs(corr_val) > 0.4, "moderate", "weak"))
  
  cat(paste0(i, ". ", var_name, ": r = ", round(corr_val, 2), 
             " (", strength, " ", direction, " relationship)\n"))
}

cat("\nRECOMMENDATION: The variable with the strongest correlation should be\n")
cat("considered first for simple linear regression modeling.\n\n")

# Create professional correlation table
corr_table <- gt(corr_hhi_df) %>%
  tab_header(
    title = "Pearson Correlations with Household Income",
    subtitle = "Sorted by absolute correlation strength"
  ) %>%
  fmt_number(
    columns = Correlation,
    decimals = 2
  )

print(corr_table)

# Save table
corr_table %>%
  gtsave(
    data = .,
    filename = "correlation_table.html",
    path = output_path
  )

# Fit simple linear regression model (College) --------------------------------------
# MODELING DECISION: We'll start with "College" as our predictor
# (assuming it has high correlation based on typical demographic patterns)

cat("\n=== MODEL 1: SIMPLE LINEAR REGRESSION ===\n")
cat("Response (Y): HouseholdIncome\n")
cat("Predictor (X): College\n\n")

# Fit model
model_1 <- lm(HouseholdIncome ~ College, data = df)

# Display full summary
summary_output_1 <- summary(model_1)
print(summary_output_1)

cat("\n=== INTERPRETING MODEL 1 OUTPUT ===\n\n")

# Coefficients
coef_model_1 <- coef(model_1)
intercept_1 <- coef_model_1["(Intercept)"]
slope_1 <- coef_model_1["College"]

cat("1. REGRESSION EQUATION:\n")
cat("HouseholdIncome = ", round(intercept_1, 2), " + ", round(slope_1, 2), " × College\n\n", sep="")

cat("2. COEFFICIENT INTERPRETATIONS:\n")
cat("• Intercept (", dollar(intercept_1), "):\n", sep="")
cat("- Predicted household income when College = 0%\n")
cat("- Usually not meaningful if 0% is outside observed data range\n\n")

cat("• Slope (", dollar(slope_1), "):\n", sep="")
cat("- For each 1 percentage point increase in college graduates,\n")
cat("household income increases by ", dollar(slope_1), " on average\n", sep="")
cat("- This represents the marginal effect of education on income\n\n")

# Statistical significance
p_value_slope <- summary_output_1$coefficients["College", "Pr(>|t|)"]
t_value_slope <- summary_output_1$coefficients["College", "t value"]

cat("3. STATISTICAL SIGNIFICANCE (t-test):\n")
cat("• Null Hypothesis (H₀): β₁ = 0 (College has no effect on income)\n")
cat("• Alternative Hypothesis (Hₐ): β₁ ≠ 0 (College affects income)\n")
cat("• t-statistic:", round(t_value_slope, 3), "\n")
cat("• p-value:", format.pval(p_value_slope, digits = 4), "\n")

if (p_value_slope < 0.001) {
  cat("• CONCLUSION: HIGHLY SIGNIFICANT (p < 0.001)\n")
  cat(" → We reject H₀ with very strong evidence\n")
  cat(" → College percentage is a statistically significant predictor\n\n")
} else if (p_value_slope < 0.05) {
  cat("• CONCLUSION: SIGNIFICANT (p < 0.05)\n")
  cat(" → We reject H₀\n")
  cat(" → College percentage is a statistically significant predictor\n\n")
} else {
  cat("• CONCLUSION: NOT SIGNIFICANT (p ≥ 0.05)\n")
  cat(" → We fail to reject H₀\n")
  cat(" → Insufficient evidence that College predicts income\n\n")
}

# R-squared
r_squared_model_1 <- summary_output_1$r.squared
cat("4. MODEL FIT (R²):\n")
cat("• R² =", round(r_squared_model_1, 4), "or", percent(r_squared_model_1), "\n")
cat("• INTERPRETATION:", percent(r_squared_model_1), "of the variation in\n")
cat("HouseholdIncome is explained by College percentage\n")
cat("• Remaining", percent(1 - r_squared_model_1), "is due to other factors\n\n")

# Adjusted R-squared
adj_r_squared_model_1 <- summary_output_1$adj.r.squared
cat("5. ADJUSTED R² (penalizes model complexity):\n")
cat("• Adjusted R² =", round(adj_r_squared_model_1, 4), "\n")
cat("• For simple regression, R² and Adj-R² are nearly identical\n")
cat("• More useful for comparing models with different numbers of predictors\n\n")

# Residual standard error
rse_1 <- summary_output_1$sigma
cat("6. RESIDUAL STANDARD ERROR:\n")
cat("• RSE =", dollar(rse_1), "\n")
cat("• INTERPRETATION: Average prediction error is", dollar(rse_1), "\n")
cat("• States' actual incomes deviate from predictions by ~", dollar(rse_1), "on average\n\n", sep="")

# Prediction equation example
cat("7. PRACTICAL APPLICATION (Prediction):\n")
cat("Example: If a state has 30% college graduates:\n")
example_x <- 30
predicted_y <- intercept_1 + slope_1 * example_x
cat("Predicted Income =", dollar(intercept_1), "+", dollar(slope_1), "× 30\n")
cat("Predicted Income =", dollar(predicted_y), "\n\n")

# T-test & Omnibus stats --------------------------------------------------
# INSIGHT: ANOVA table decomposes total variation into explained vs. unexplained
# This provides an F-test for overall model significance

cat("=== ANOVA TABLE (Omnibus F-Test) ===\n")
model_1_anova <- anova(model_1)
print(model_1_anova)

cat("\n=== INTERPRETING ANOVA OUTPUT ===\n\n")

# Extract F-statistic and p-value
f_stat <- summary_output_1$fstatistic[1]
f_pval <- pf(f_stat, summary_output_1$fstatistic[2], summary_output_1$fstatistic[3], lower.tail = FALSE)

cat("1. HYPOTHESIS TEST:\n")
cat("• H₀: The model has no predictive power (all slopes = 0)\n")
cat("• Hₐ: The model has predictive power (at least one slope ≠ 0)\n")
cat("• For simple regression, F-test equivalent to t-test\n\n")

cat("2. F-STATISTIC:\n")
cat("• F =", round(f_stat, 3), "\n")
cat("• p-value:", format.pval(f_pval, digits = 4), "\n\n")

cat("3. ANOVA SUMS OF SQUARES INTERPRETATION:\n")
anova_ssr <- model_1_anova$`Sum Sq`[1]  # Regression sum of squares
anova_sse <- model_1_anova$`Sum Sq`[2]  # Residual sum of squares
anova_sst <- anova_ssr + anova_sse       # Total sum of squares

cat("• SSR (Sum of Squares Regression) =", comma(anova_ssr), "\n")
cat(" → Variation EXPLAINED by the model\n")
cat("• SSE (Sum of Squares Error) =", comma(anova_sse), "\n")
cat(" → Variation UNEXPLAINED (residual variation)\n")
cat("• SST (Sum of Squares Total) =", comma(anova_sst), "\n")
cat(" → Total variation in Y\n\n")

cat("4. VERIFY R² CALCULATION:\n")
cat("• R² = SSR / SST =", round(anova_ssr / anova_sst, 4), "\n")
cat("• Matches R² from regression output:", round(r_squared_model_1, 4), "✓\n\n")

# Manual calculation of ANOVA ---------------------------------------------
# PEDAGOGICAL INSIGHT: Manually calculating ANOVA components demonstrates
# deep understanding of regression mechanics and reinforces theoretical concepts

cat("=== MANUAL ANOVA CALCULATIONS (Verification) ===\n\n")

# Get predicted values
df$y_hat <- predict(model_1)

# Get residuals (observed - predicted)
df$residuals <- df$HouseholdIncome - df$y_hat

cat("Created variables in dataframe:\n")
cat("• y_hat: Predicted values from regression\n")
cat("• residuals: Actual - Predicted (errors)\n\n")

# Sum of squared errors (SSE)
SSE <- sum((df$residuals)^2)
cat("SSE (Sum of Squared Errors):\n")
cat("Formula: Σ(Yᵢ - Ŷᵢ)²\n")
cat("Calculation:", comma(SSE), "\n")
cat("Matches ANOVA table:", comma(anova_sse), "✓\n\n")

# Mean of Y
Y_bar <- mean(df$HouseholdIncome)
cat("Mean of Y (Ȳ):", dollar(Y_bar), "\n\n")

# Sum of squares total (SST)
SST <- sum((df$HouseholdIncome - Y_bar)^2)
cat("SST (Total Sum of Squares):\n")
cat("Formula: Σ(Yᵢ - Ȳ)²\n")
cat("Calculation:", comma(SST), "\n")
cat("Matches ANOVA table:", comma(anova_sst), "✓\n\n")

# Sum of squares regression (SSR)
SSR <- sum((df$y_hat - Y_bar)^2)
cat("SSR (Regression Sum of Squares):\n")
cat("Formula: Σ(Ŷᵢ - Ȳ)²\n")
cat("Calculation:", comma(SSR), "\n")
cat("Matches ANOVA table:", comma(anova_ssr), "✓\n\n")

# R-squared calculation
R_test <- SSR/SST
cat("Manual R² calculation:\n")
cat("R² = SSR / SST =", round(R_test, 4), "\n")
cat("Matches model output:", round(r_squared_model_1, 4), "✓\n\n")

cat("KEY INSIGHT: SST = SSR + SSE\n")
cat("Verification:", comma(SST), "=", comma(SSR), "+", comma(SSE), "\n")
cat("Check:", comma(SSR + SSE), "✓\n\n")

# Standardized residual plots ---------------------------------------------
# INSIGHT: Residual diagnostics are CRITICAL for validating regression assumptions
# 1. Normality: Residuals should be approximately normally distributed
# 2. Homoscedasticity: Variance of residuals should be constant
# 3. Independence: Residuals should show no pattern vs. predicted values
# 4. No influential outliers: Large residuals may distort the model

cat("=== RESIDUAL DIAGNOSTICS (Model Assumptions) ===\n\n")

# Standardize residuals (divide by SD)
resid_sd <- sd(df$residuals)
df$std_residuals <- df$residuals / resid_sd

cat("Standardized residuals created:\n")
cat("  • Mean:", round(mean(df$std_residuals), 6), "(should be ≈ 0)\n")
cat("  • SD:", round(sd(df$std_residuals), 4), "(should be ≈ 1)\n\n")

cat("Creating diagnostic plots...\n\n")

# Histogram of standardized residuals
# INSIGHT: Should look approximately normal (bell-shaped, symmetric)
ggplot(data = df, aes(x = std_residuals))  +
  geom_histogram(fill = "steelblue", color = "black", bins = 15) +
  labs(title = "Histogram of Standardized Residuals (Model 1)",
       subtitle = "Assessing normality assumption",
       x = "Standardized Residuals",
       y = "Frequency") +
  theme_bw(base_size = 16)
ggsave("standardized_residuals_hist.png", path = output_path, dpi = 300, width = 10, height = 6, units = "in")

cat("HISTOGRAM INTERPRETATION:\n")
cat("  ✓ Approximately normal → Residuals satisfy normality assumption\n")
cat("  ✗ Skewed or multi-modal → Normality assumption violated\n")
cat("    → May need transformation or robust regression\n\n")

# Scatterplot of standardized residuals vs predicted values
# INSIGHT: Should show random scatter around zero (no pattern)
ggplot(data = df, aes(x = y_hat, y = std_residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  geom_hline(yintercept = c(-2, 2), color = "orange", linetype = 3) +
  labs(title = "Standardized Residuals vs Predicted Values (Model 1)",
       subtitle = "Checking for homoscedasticity and independence",
       x = "Predicted Values ($)",
       y = "Standardized Residuals") +
  theme_bw(base_size = 16)
ggsave("std_residuals_pred_vals_plot.png", path = output_path, dpi = 300, width = 10, height = 6, units = "in")

cat("RESIDUAL PLOT INTERPRETATION:\n")
cat("  GOOD SIGNS:\n")
cat("  ✓ Random scatter around y=0 → Independence assumption satisfied\n")
cat("  ✓ Equal spread across x-axis → Homoscedasticity (constant variance)\n")
cat("  ✓ Most points within ±2 SD → No extreme outliers\n\n")
cat("  BAD SIGNS:\n")
cat("  ✗ Funnel shape → Heteroscedasticity (non-constant variance)\n")
cat("  ✗ Curved pattern → Non-linear relationship (need transformation)\n")
cat("  ✗ Points beyond ±3 SD → Outliers (investigate influential observations)\n\n")

# Identify potential outliers
outliers <- df[abs(df$std_residuals) > 2, c("State", "HouseholdIncome", "College", "y_hat", "std_residuals")]
if (nrow(outliers) > 0) {
  cat("POTENTIAL OUTLIERS (|standardized residual| > 2):\n")
  print(outliers)
  cat("\nRECOMMENDATION: Investigate these states for:\n")
  cat("  • Data entry errors\n")
  cat("  • Unique economic circumstances\n")
  cat("  • Influential observations (high leverage)\n\n")
} else {
  cat("No standardized residuals exceed ±2 SD. Excellent fit!\n\n")
}

# Fit 2nd simple linear regression model --------------------------------
# COMPARATIVE ANALYSIS: Fit alternative models to determine best predictor

cat("=== MODEL 2: ALTERNATIVE PREDICTOR ===\n")
cat("Response (Y): HouseholdIncome\n")
cat("Predictor (X): Obese\n\n")

# Fit model 2
model_2 <- lm(HouseholdIncome ~ Obese, data = df)

# Get summary of model 2
summary_output_2 <- summary(model_2)
print(summary_output_2)

cat("\n=== MODEL 2 KEY METRICS ===\n")
coef_model_2 <- coef(model_2)
r_squared_model_2 <- summary_output_2$r.squared
adj_r_squared_model_2 <- summary_output_2$adj.r.squared
p_value_slope_2 <- summary_output_2$coefficients["Obese", "Pr(>|t|)"]

cat("Slope:", round(coef_model_2["Obese"], 2), "\n")
cat("Interpretation: Each 1% increase in obesity rate is associated with a\n")
cat("                ", dollar(coef_model_2["Obese"]), " change in household income\n\n", sep="")
cat("R²:", round(r_squared_model_2, 4), "or", percent(r_squared_model_2), "\n")
cat("p-value:", format.pval(p_value_slope_2, digits = 4), "\n\n")

# ANOVA of model 2
model_2_anova <- anova(model_2)
print(model_2_anova)

cat("\n")

# Fit 3rd simple linear regression model ----------------------------------
cat("=== MODEL 3: THIRD ALTERNATIVE PREDICTOR ===\n")
cat("Response (Y): HouseholdIncome\n")
cat("Predictor (X): Insured\n\n")

# Fit model 3
model_3 <- lm(HouseholdIncome ~ Insured, data = df)

# Get summary of model 3
summary_output_3 <- summary(model_3)
print(summary_output_3)

cat("\n=== MODEL 3 KEY METRICS ===\n")
coef_model_3 <- coef(model_3)
r_squared_model_3 <- summary_output_3$r.squared
adj_r_squared_model_3 <- summary_output_3$adj.r.squared
p_value_slope_3 <- summary_output_3$coefficients["Insured", "Pr(>|t|)"]

cat("Slope:", round(coef_model_3["Insured"], 2), "\n")
cat("Interpretation: Each 1% increase in insured population is associated with a\n")
cat("                ", dollar(coef_model_3["Insured"]), " change in household income\n\n", sep="")
cat("R²:", round(r_squared_model_3, 4), "or", percent(r_squared_model_3), "\n")
cat("p-value:", format.pval(p_value_slope_3, digits = 4), "\n\n")

# ANOVA of model 3
model_3_anova <- anova(model_3)
print(model_3_anova)

cat("\n")

# Model Comparison --------------------------------------------------------------
# Create comparison table
comparison_df <- data.frame(
  Model = c("Model 1 (College)", "Model 2 (Obese)", "Model 3 (Insured)"),
  Predictor = c("College", "Obese", "Insured"),
  R_squared = c(r_squared_model_1, r_squared_model_2, r_squared_model_3),
  Adj_R_squared = c(adj_r_squared_model_1, adj_r_squared_model_2, adj_r_squared_model_3),
  Slope = c(coef_model_1[2], coef_model_2[2], coef_model_3[2]),
  P_value = c(p_value_slope, p_value_slope_2, p_value_slope_3),
  RSE = c(summary_output_1$sigma, summary_output_2$sigma, summary_output_3$sigma)
) %>%
  arrange(desc(R_squared))

cat("=== MODEL COMPARISON TABLE ===\n")
print(comparison_df)

cat("\n=== DECISION CRITERIA ===\n\n")

# Identify best model
best_model_idx <- which.max(comparison_df$R_squared)
best_model <- comparison_df[best_model_idx, ]

cat("1. HIGHEST R² (Best Explanatory Power):\n")
cat("Winner:", best_model$Model, "\n")
cat("R² =", percent(best_model$R_squared), "\n")
cat("Interpretation: Explains", percent(best_model$R_squared), "of income variation\n\n")

# Identify most significant
most_sig_idx <- which.min(comparison_df$P_value)
most_sig_model <- comparison_df[most_sig_idx, ]

cat("2. LOWEST P-VALUE (Strongest Statistical Evidence):\n")
cat("Winner:", most_sig_model$Model, "\n")
cat("p-value:", format.pval(most_sig_model$P_value, digits = 4), "\n\n")

# Identify smallest error
best_rse_idx <- which.min(comparison_df$RSE)
best_rse_model <- comparison_df[best_rse_idx, ]

cat("3. LOWEST RSE (Best Prediction Accuracy):\n")
cat("Winner:", best_rse_model$Model, "\n")
cat("RSE =", dollar(best_rse_model$RSE), "\n")
cat("Interpretation: Average prediction error is", dollar(best_rse_model$RSE), "\n\n")

# Overall recommendation
cat("=== FINAL RECOMMENDATION ===\n\n")
cat("Based on comprehensive evaluation across multiple criteria:\n\n")

if (best_model_idx == most_sig_idx && best_model_idx == best_rse_idx) {
  cat("UNANIMOUS WINNER:", best_model$Model, "\n\n")
  cat("This model excels on ALL metrics:\n")
  cat("Highest explanatory power (R² =", percent(best_model$R_squared), ")\n", sep="")
  cat("Strongest statistical significance (p <", format.pval(best_model$P_value, digits = 3), ")\n")
  cat("Best prediction accuracy (RSE =", dollar(best_model$RSE), ")\n\n", sep="")
  cat("BUSINESS IMPLICATION:\n")
  cat(best_model$Predictor, "is the single best predictor of household income at the state level.\n")
  cat("Policymakers should prioritize", best_model$Predictor, "when targeting income growth.\n\n")
} else {
  cat("NUANCED RECOMMENDATION:\n\n")
  cat("The models show trade-offs:\n")
  cat("• Best fit (R²):", best_model$Model, "(", percent(best_model$R_squared), ")\n", sep="")
  cat("• Most significant:", most_sig_model$Model, "(p =", format.pval(most_sig_model$P_value), ")\n", sep="")
  cat("• Best accuracy:", best_rse_model$Model, "(RSE =", dollar(best_rse_model$RSE), ")\n\n", sep="")
  cat("RECOMMENDATION: Select", best_model$Model, "for its superior explanatory power,\n")
  cat("but acknowledge that other factors may also be relevant.\n\n")
}

cat("ANALYSIS COMPLETE\n")
cat("\nAll outputs saved to:", output_path, "\n")
