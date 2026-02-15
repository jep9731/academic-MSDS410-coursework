################################################################################
# MSDS 410 - Assignment 5: ANOVA, ANCOVA, and Categorical Variables
# Author: Joshua Pasaye
# Purpose: Demonstrate analysis with categorical predictors, including ANOVA,
#          dummy variable coding, ANCOVA (parallel slopes), interaction effects
#          (unequal slopes), and comprehensive model comparison
# 
# KEY INSIGHTS DEMONSTRATED:
# 1. One-way ANOVA for comparing group means
# 2. Dummy variable encoding for categorical predictors
# 3. Equivalence of ANOVA and regression with categorical predictors
# 4. ANCOVA (Analysis of Covariance) - categorical + continuous predictors
# 5. Parallel slopes vs. unequal slopes (interaction effects)
# 6. Testing for homogeneity of slopes
# 7. Effect size interpretation (eta-squared, R²)
# 8. Post-hoc pairwise comparisons (Tukey HSD)
# 9. Model selection across multiple categorical variables
# 10. Translating group differences into business insights
################################################################################

# Set-up environment ------------------------------------------------------
# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(car)
library(readxl)
# install.packages("fastDummies") # Install if needed
library(fastDummies)
library(effectsize)
library(gt)

# Import data
df <- read_xls("NutritionStudy.xls")

# Create output directory
output_dir <- "Output"

if (!dir.exists(output_dir)) {
  dir.create(path = paste(dirname(getwd()), "/Output", sep = ""))
} else {
  print("Directory already exists!")
}
# Output path
output_path <- file.path(paste(dirname(getwd()), "/Output", sep = ""))

# Descriptive Statistics (Model 1) ----------------------------------------
# For the first model, we will be using CHOLESTEROL as the response variable (Y)
# and we will use PRIORSMOKE as the predictor variable (X).
# In the first step, we will be looking at the descriptive statistics and the group means.

## Make PriorSmoke into factor since numerical factor ##
# RATIONALE: PriorSmoke column is a categorical column with integers 1, 2, and 3
# denoting category groupings. Converting to factor ensures R treats it properly
# in statistical models rather than as a continuous numeric variable.
df$PriorSmoke <- as.factor(df$PriorSmoke)

## Create summary table ##
summary_m1 <- df %>%
  # Group by PriorSmoke
  group_by(PriorSmoke) %>%
  # Calculate summary stats
  summarise(
    n = n(),
    median = median(Cholesterol),
    mean = mean(Cholesterol),
    min = min(Cholesterol),
    max = max(Cholesterol),
    var = var(Cholesterol),
    sd = sd(Cholesterol)
  ) %>%
  mutate(prop = (n / sum(n)) * 100, .after = n)
print(summary_m1)

cat("\n=== INTERPRETING DESCRIPTIVE STATISTICS ===\n\n")

# Extract key insights
for (i in 1:nrow(summary_m1)) {
  cat("Group", summary_m1$PriorSmoke[i], ":\n")
  cat("  Sample size: n =", summary_m1$n[i], 
      "(", round(summary_m1$prop[i], 1), "% of total)\n")
  cat("  Mean cholesterol:", round(summary_m1$mean[i], 1), "mg/dL\n")
  cat("  SD:", round(summary_m1$sd[i], 1), "mg/dL\n")
  cat("  Range:", summary_m1$min[i], "-", summary_m1$max[i], "mg/dL\n\n")
}

# Compare means across groups
mean_diff_1_2 <- summary_m1$mean[2] - summary_m1$mean[1]
mean_diff_1_3 <- summary_m1$mean[3] - summary_m1$mean[1]
mean_diff_2_3 <- summary_m1$mean[3] - summary_m1$mean[2]

cat("PAIRWISE MEAN DIFFERENCES (descriptive, not tested yet):\n")
cat("  Group 2 - Group 1:", round(mean_diff_1_2, 1), "mg/dL\n")
cat("  Group 3 - Group 1:", round(mean_diff_1_3, 1), "mg/dL\n")
cat("  Group 3 - Group 2:", round(mean_diff_2_3, 1), "mg/dL\n\n")

cat("INITIAL OBSERVATIONS:\n")
if (abs(mean_diff_1_3) > 10) {
  cat("  • Substantial difference between Groups 1 and 3 (", 
      round(mean_diff_1_3, 1), "mg/dL)\n")
}
if (max(summary_m1$sd) / min(summary_m1$sd) > 2) {
  cat("  ⚠ Variance heterogeneity concern (SD ratio >2)\n")
  cat("    May violate ANOVA homogeneity assumption\n")
} else {
  cat("  ✓ Standard deviations relatively similar across groups\n")
  cat("    Homogeneity of variance assumption likely satisfied\n")
}
cat("\n")


# Create report ready table
summary_m1 %>%
  gt() %>%
  tab_header(
    title = "Summary Statistics by Prior Smoking Status",
    subtitle = "Cholesterol Levels"
  ) %>%
  cols_label(
    PriorSmoke = "Prior Smoke",
    n = "N",
    prop = "Prop (%)",
    median = "Median",
    mean = "Mean",
    min = "Min",
    max = "Max",
    var = "Variance",
    sd = "SD"
  ) %>%
  fmt_number(
    columns = c(mean, median, min, max, sd),
    decimals = 1
  ) %>%
  fmt_number(
    columns = var,
    decimals = 0
  ) %>%
  fmt_number(
    columns = prop,
    decimals = 1
  ) %>%
  tab_style(
    style = cell_fill(color = "#4472C4"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_footnote(
    footnote = "N = sample size; Prop = proportion; SD = standard deviation."
  )

# Transform Categorical variables -----------------------------------------
# Determine number of categorical cols
cat("There are", sum(sapply(df, is.character) | sapply(df, is.factor)), "categorical columns the NutritionStudy dataset.\n")
cat("We will create dummy variables for the following columns:", 
    paste(names(df[sapply(df, is.character) | sapply(df, is.factor)]), collapse = ", "))

# Use fastDummies dummy_cols()
df_t <- dummy_cols(df, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Print results
str(df_t)
cat("The dummy variables have been created and the dataset is ready for modeling!\n")

# ANOVA Model ----------------------------------------------------------
cat("=== ANOVA vs REGRESSION: TWO APPROACHES, SAME TEST ===\n\n")

cat("ANOVA APPROACH:\n")
cat("  • Classical framework for comparing group means\n")
cat("  • Tests if variance between groups exceeds variance within groups\n")
cat("  • Uses aov() function in R\n\n")

cat("REGRESSION APPROACH:\n")
cat("  • Uses dummy variables to represent group membership\n")
cat("  • Tests if dummy coefficients are jointly significant\n")
cat("  • Uses lm() function in R\n\n")

cat("KEY INSIGHT: Both approaches yield IDENTICAL F-statistics and p-values!\n")
cat("  • ANOVA is special case of regression with categorical predictors\n")
cat("  • Regression offers more flexibility (can add continuous covariates)\n\n")

## Fit ANOVA model ##
anova_model <- aov(Cholesterol ~ PriorSmoke, data = df)

# Get ANOVA table
anova_summary <- summary(anova_model)
print(anova_summary)

# Extract statistics
f_stat <- anova_summary[[1]]$`F value`[1]
p_value <- anova_summary[[1]]$`Pr(>F)`[1]
df_between <- anova_summary[[1]]$Df[1]
df_within <- anova_summary[[1]]$Df[2]

cat(sprintf("\nF(%d, %d) = %.4f, p = %.4f\n", df_between, df_within, f_stat, p_value))

## Interpret significance ##
if (p_value < 0.05) {
  cat(sprintf("Result: REJECT the null hypothesis (p = %.4f < 0.05)\n\n", p_value))
  cat("Interpretation:\n")
  cat("  There is statistically significant evidence that mean Cholesterol levels\n")
  cat("  differ across PRIORSMOKE groups. At least two groups have significantly\n")
  cat("  different mean Cholesterol levels.\n\n")
  
  cat("Group Means:\n")
  for (i in 1:nrow(summary_m1)) {
    cat(sprintf("  %s: M = %.2f mg/dL (SD = %.2f, n = %d)\n",
                summary_m1$PriorSmoke[i], summary_m1$mean[i], 
                summary_m1$sd[i], summary_m1$n[i]))
  }
  
  cat("\n  Post-hoc tests indicate which specific pairs of groups differ.\n")
  
} else {
  cat(sprintf("Result: FAIL TO REJECT the null hypothesis (p = %.4f >= 0.05)\n\n", p_value))
  cat("Interpretation:\n")
  cat(" There is insufficient evidence to conclude that mean Cholesterol levels\n")
  cat(" differ across PRIORSMOKE groups.\n")
}

## Interpret Coefficients ##
# Intercept = Mean for Group 1 (reference: PriorSmoke_2=0, PriorSmoke_3=0)
# PriorSmoke_2 coefficient = Difference between Group 2 and Group 1
# PriorSmoke_3 coefficient = Difference between Group 3 and Group 1

cat("\nGroup Means (calculated from model):\n")
cat(sprintf("  Group 1: %.2f mg/dL\n", coef(anova_model)[1]))
cat(sprintf("  Group 2: %.2f mg/dL\n", coef(anova_model)[1] + coef(anova_model)[2]))
cat(sprintf("  Group 3: %.2f mg/dL\n", coef(anova_model)[1] + coef(anova_model)[3]))

## Effect size ##
eta_squared(anova_model, partial = FALSE)

## Post-hoc tests ##
if (p_value < 0.05) {
  cat("\n\n3. POST-HOC TESTS (Tukey HSD)\n")
  tukey_results <- TukeyHSD(anova_model)
  print(tukey_results)
}

# Model 1 -----------------------------------------------------------------
## Fit Model 1 ##
model_1 <- lm(Cholesterol ~ PriorSmoke_2 + PriorSmoke_3, data = df_t)

# Model Summary
print(summary(model_1))

## Extract coefficients ##
coef_table <- summary(model_1)$coefficients
beta_0 <- coef_table[1, 1]  # Intercept (PriorSmoke_1/Reference Group)
beta_1 <- coef_table[2, 1]  # PriorSmoke_2
beta_2 <- coef_table[3, 1]  # PriorSmoke_3

## Prediction Equation ##
cat(sprintf("Ŷ = %.3f + %.3f(PriorSmoke_2) + %.3f(PriorSmoke_3)\n\n", 
            beta_0, beta_1, beta_2))

## ANOVA Table for Regression Model ##
anova_reg <- anova(model_1)
print(anova_reg)

# Extract regression ANOVA statistics
f_stat_reg <- summary(model_1)$fstatistic[1]
df1_reg <- summary(model_1)$fstatistic[2]
df2_reg <- summary(model_1)$fstatistic[3]
p_value_reg <- pf(f_stat_reg, df1_reg, df2_reg, lower.tail = FALSE)

cat(sprintf("\nOverall Model F-test: F(%d, %d) = %.4f, p = %.4f\n", 
            df1_reg, df2_reg, f_stat_reg, p_value_reg))
cat(sprintf("R² = %.4f (%.2f%% of variance explained)\n", 
            summary(model_1)$r.squared, summary(model_1)$r.squared * 100))
cat(sprintf("Adjusted R² = %.4f\n", summary(model_1)$adj.r.squared))

## Comparison of Reg Model & ANOVA ##
cat(sprintf("Overall F-statistic: ANOVA = %.4f, Regression = %.4f ✓\n", 
            f_stat, f_stat_reg))
cat(sprintf("Overall p-value:     ANOVA = %.4f, Regression = %.4f ✓\n",
            p_value, p_value_reg))
cat(sprintf("Degrees of freedom:  df(%d, %d) for both ✓\n", df_between, df_within))

## Visualizations ##
png(paste0(output_path, "/cholesterol_anova_plot.png"), 
    units = "in", width = 12, height = 7, res = 300)
par(mfrow = c(1, 2))

# 1. Boxplot
boxplot(Cholesterol ~ PriorSmoke, data = df,
        main = "Cholesterol by PriorSmoke Group",
        xlab = "PriorSmoke Group",
        ylab = "Cholesterol (mg/dL)",
        col = c("lightblue", "lightgreen", "lightcoral"),
        names = c("1", "2", "3"))

# Add means
means <- tapply(df$Cholesterol, df$PriorSmoke, mean)
points(1:3, means, pch = 19, col = "gray20", cex = 1.5)
legend("topleft", legend = "Mean", pch = 19, col = "gray20", bty = "n")

# 2. Bar plot with error bars
sds <- tapply(df$Cholesterol, df$PriorSmoke, sd)
ns <- tapply(df$Cholesterol, df$PriorSmoke, length)
sems <- sds / sqrt(ns)

bp <- barplot(means,
              main = "Mean Cholesterol ± SEM",
              xlab = "PriorSmoke Group",
              ylab = "Mean Cholesterol (mg/dL)",
              col = c("steelblue", "darkorange", "darkgreen"),
              names.arg = c("1", "2", "3"),
              ylim = c(0, max(means + sems) * 1.1))
arrows(bp, means - sems, bp, means + sems,
       angle = 90, code = 3, length = 0.1, lwd = 2)
text(bp, means + sems, labels = round(means, 1), pos = 3, cex = 0.9)

# Save plots
dev.off()

# ANCOVA (model 2) --------------------------------------------------------
## Fit model 2 ##
model_2 <- lm(Cholesterol ~ PriorSmoke_2 + PriorSmoke_3 + Fat, data = df_t)

# Model Summary
print(summary(model_2))

## Extract coefficients ##
coef_table <- summary(model_2)$coefficients
beta_0 <- coef_table[1, 1] # Intercept (PriorSmoke_1/Reference Group)
beta_1 <- coef_table[2, 1] # PriorSmoke_2
beta_2 <- coef_table[3, 1] # PriorSmoke_3
beta_3 <- coef_table[4, 1] # Fat

## Prediction Equation ##
cat(sprintf("Ŷ = %.3f + %.3f(PriorSmoke_2) + %.3f(PriorSmoke_3) + %.3f(Fat)\n\n", 
            beta_0, beta_1, beta_2, beta_3))

## ANOVA Table for Regression Model ##
anova_reg <- anova(model_2)
print(anova_reg)

# Extract regression ANOVA statistics
f_stat_reg <- summary(model_2)$fstatistic[1]
df1_reg <- summary(model_2)$fstatistic[2]
df2_reg <- summary(model_2)$fstatistic[3]
p_value_reg <- pf(f_stat_reg, df1_reg, df2_reg, lower.tail = FALSE)

cat(sprintf("\nOverall Model F-test: F(%d, %d) = %.4f, p = %.4f\n", 
            df1_reg, df2_reg, f_stat_reg, p_value_reg))
cat(sprintf("R² = %.4f (%.2f%% of variance explained)\n", 
            summary(model_2)$r.squared, summary(model_2)$r.squared * 100))
cat(sprintf("Adjusted R² = %.4f\n", summary(model_2)$adj.r.squared))

# Model 2 Assumption Checks --------------------------------------------------------
## Normality ##
# Extract residuals from the model
residuals_model2 <- residuals(model_2)

# Shapiro-Wilk test
shapiro_result <- shapiro.test(residuals_model2)
print(shapiro_result)

if (shapiro_result$p.value > 0.05) {
  cat("   ✓ Assumption met: Residuals are normally distributed\n")
} else {
  cat("   ✗ Assumption violated: Residuals are not normally distributed\n")
}

# Q-Q plot
png(paste0(output_path, "/qqplot_m2.png"),
    units = "in", width = 12, height = 7.5, res = 300)
qqPlot(model_2, main = "Q-Q Plot (Model 2)")
dev.off()
qqPlot(model_2, main = "Q-Q Plot (Model 2)")

# INTERPRETATION: The normality assumption does not seem to be met.
# There are several deviations from the normal line as the studentized residuals increases to +2.
# Additionally, there may be some severe outliers at observations 94 and 112.

## Homogeneity of variance ##
# Non-constant variance test
ncv_test <- ncvTest(model_2)
print(ncv_test)

if (ncv_test$p > 0.05) {
  cat("   ✓ Assumption met: Variance is constant\n")
} else {
  cat("   ✗ Assumption violated: Variance is not constant\n")
}

# Plot residuals vs fitted values
# Get residuals
df_t$residuals_m2 <- residuals(model_2)

# Get predicted values
df_t$pred_vals_m2 <- fitted.values(model_2)

# Plot
ggplot(data = df_t, aes(x = pred_vals_m2, y = residuals_m2)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  labs(title = "Residuals vs Predicted Values (Model 2)",
       x = "Predicted values",
       y = "Residuals") +
  theme_bw(base_size = 14)
ggsave("resids_pred_vals_model_2.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# INTERPRETATION: The homogeneity of variance (homoscedasticity) assumption is not met.
# There is a large spread of the residuals as the fitted values increases past 300.

## Homogeneity of regression slopes ##
# Fit model WITH interaction (non-parallel slopes)
model_interaction <- lm(Cholesterol ~ PriorSmoke_2 + PriorSmoke_3 + Fat + 
                          PriorSmoke_2:Fat + PriorSmoke_3:Fat, data = df_t)

# View the interaction model
print(summary(model_interaction))

# Compare models with and without interaction using ANOVA
slope_test <- anova(model_2, model_interaction)
print(slope_test)

if (slope_test$`Pr(>F)`[2] > 0.05) {
  cat("\n   ✓ Assumption met: Regression slopes are homogeneous (p = ", 
      round(slope_test$`Pr(>F)`[2], 4), ")\n", sep = "")
  cat("   The relationship between Fat and Cholesterol is similar across smoking groups.\n")
} else {
  cat("\n   ✗ Assumption violated: Regression slopes differ (p = ", 
      round(slope_test$`Pr(>F)`[2], 4), ")\n", sep = "")
  cat("   The relationship between Fat and Cholesterol varies by smoking status.\n")
}

# INTERPRETATION: The assumption of homogeneity of regression slopes is not valid.
# This means that the effect of 'Fat' on Cholesterol is different across the groups.

## Outlier test ##
outlierTest(model = model_2)
outlier_results_m2 <- data.frame(
  rstudent = outlierTest(model_2)$rstudent,
  unadjusted_p = outlierTest(model_2)$p,
  bonferroni_p = outlierTest(model_2)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_m2, file = paste0(output_path, "/outlier_results_table_m2.csv"), row.names = TRUE)

# Scatterplot Comparison --------------------------------------------------
## Get predicted  values of Model 2 ##
df$pred_vals_m2 <- fitted.values(model_2)

## Plot of fitted values vs fat by smoking status ##
ggplot(data = df, aes(x = Fat, y = pred_vals_m2, color = PriorSmoke)) +
  geom_point() +
  labs(title = "Predicted Values vs Fat (Model 2)",
       x = "Fat",
       y = "Predicted Values",
       color = "Smoking Status") +
  scale_color_manual(
    values = c("#606c38", "#bc6c25", "#22223b"), 
    labels = c("Group 1", "Group 2", "Group 3")
  ) +
  theme_bw(base_size = 14)
ggsave("fat_pred_vals_model_2.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

## Plot of actual cholesterol vs fat by smoking status ##
ggplot(data = df, aes(x = Fat, y = Cholesterol, color = PriorSmoke)) +
  geom_point() +
  labs(title = "Cholesterol vs Fat (Model 2)",
       x = "Fat",
       y = "Cholesterol",
       color = "Smoking Status") +
  scale_color_manual(
    values = c("#ccd5ae", "#d4a373", "#023e8a"), 
    labels = c("Group 1", "Group 2", "Group 3")
  ) +
  theme_bw(base_size = 14)
ggsave("fat_cholesterol_model_2.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# Unequal Slopes Model (Model 3) -----------------------------------------------------------------
## Create product variables ##
# PriorSmoke_2 x Fat
df_t$`PriorSmoke_2:Fat` <- df_t$PriorSmoke_2 * df$Fat

# PriorSmoke_3 x Fat
df_t$`PriorSmoke_3:Fat` <- df_t$PriorSmoke_3 * df$Fat

## Fit Model 3 ##
model_3 <- lm(Cholesterol ~ PriorSmoke_2 + PriorSmoke_3 + Fat + `PriorSmoke_2:Fat` + `PriorSmoke_3:Fat`,
              data = df_t)

# Model Summary
print(summary(model_3))

## Extract coefficients ##
coef_table <- summary(model_3)$coefficients
beta_0 <- coef_table[1, 1] # Intercept (PriorSmoke_1/Reference Group)
beta_1 <- coef_table[2, 1] # PriorSmoke_2
beta_2 <- coef_table[3, 1] # PriorSmoke_3
beta_3 <- coef_table[4, 1] # Fat
beta_4 <- coef_table[5, 1] # PriorSmoke_2:Fat
beta_5 <- coef_table[6, 1] # PriorSmoke_3:Fat

## Prediction Equation ##
cat(sprintf("Ŷ = %.3f + %.3f(PriorSmoke_2) + %.3f(PriorSmoke_3) + %.3f(Fat) +
            %.3f(PriorSmoke_2:Fat) + %.3f(PriorSmoke_3:Fat)\n\n", 
            beta_0, beta_1, beta_2, beta_3, beta_4, beta_5))

## ANOVA Table for Regression Model ##
anova_reg <- anova(model_3)
print(anova_reg)

# Extract regression ANOVA statistics
f_stat_reg <- summary(model_3)$fstatistic[1]
df1_reg <- summary(model_3)$fstatistic[2]
df2_reg <- summary(model_3)$fstatistic[3]
p_value_reg <- pf(f_stat_reg, df1_reg, df2_reg, lower.tail = FALSE)

cat(sprintf("\nOverall Model F-test: F(%d, %d) = %.4f, p = %.4f\n", 
            df1_reg, df2_reg, f_stat_reg, p_value_reg))
cat(sprintf("R² = %.4f (%.2f%% of variance explained)\n", 
            summary(model_3)$r.squared, summary(model_3)$r.squared * 100))
cat(sprintf("Adjusted R² = %.4f\n", summary(model_3)$adj.r.squared))

# Model 3 Assumption Checks --------------------------------------------------------
## Normality ##
# Extract residuals from the model
residuals_model3 <- residuals(model_3)

# Shapiro-Wilk test
shapiro_result <- shapiro.test(residuals_model3)
print(shapiro_result)

if (shapiro_result$p.value > 0.05) {
  cat("   ✓ Assumption met: Residuals are normally distributed\n")
} else {
  cat("   ✗ Assumption violated: Residuals are not normally distributed\n")
}

# Q-Q plot
png(paste0(output_path, "/qqplot_m3.png"),
    units = "in", width = 12, height = 7.5, res = 300)
qqPlot(model_3, main = "Q-Q Plot (Model 3)")
dev.off()
qqPlot(model_3, main = "Q-Q Plot (Model 3)")

# INTERPRETATION: The normality assumption does not seem to be met.
# There are several deviations from the normal line as the studentized residuals increases to +2.
# Additionally, there may be some severe outliers at observations 94 and 112.

## Homogeneity of variance ##
# Non-constant variance test
ncv_test <- ncvTest(model_3)
print(ncv_test)

if (ncv_test$p > 0.05) {
  cat("   ✓ Assumption met: Variance is constant\n")
} else {
  cat("   ✗ Assumption violated: Variance is not constant\n")
}

# Plot residuals vs fitted values
# Get residuals
df_t$residuals_m3 <- residuals(model_3)

# Get predicted values
df_t$pred_vals_m3 <- fitted.values(model_3)

# Plot
ggplot(data = df_t, aes(x = pred_vals_m3, y = residuals_m3)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = 2) +
  labs(title = "Residuals vs Predicted Values (Model 3)",
       x = "Predicted values",
       y = "Residuals") +
  theme_bw(base_size = 14)
ggsave("resids_pred_vals_model_3.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# INTERPRETATION: The homogeneity of variance (homoscedasticity) assumption is not met.
# There is a large spread of the residuals as the fitted values increases past 300.

## Outlier test ##
outlierTest(model = model_3)
outlier_results_m3 <- data.frame(
  rstudent = outlierTest(model_3)$rstudent,
  unadjusted_p = outlierTest(model_3)$p,
  bonferroni_p = outlierTest(model_3)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_m3, file = paste0(output_path, "/outlier_results_table_m3.csv"), row.names = TRUE)

## Scatterplot ##
## Get predicted  values of Model 2 ##
df$pred_vals_m3 <- fitted.values(model_3)

## Plot of fitted values vs fat by smoking status ##
ggplot(data = df, aes(x = Fat, y = pred_vals_m3, color = PriorSmoke)) +
  geom_point() +
  labs(title = "Predicted Values vs Fat (Model 3)",
       x = "Fat",
       y = "Predicted Values",
       color = "Smoking Status") +
  scale_color_manual(
    values = c("#4682B4", "#B4464B", "#B4AF46"), 
    labels = c("Group 1", "Group 2", "Group 3")
  ) +
  theme_bw(base_size = 14)
ggsave("fat_pred_vals_model_3.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# Nested F-test -----------------------------------------------------------
# RATIONALE: Model 2 and Model 3 are nested models because Model 3 has all
# the variables in Model 2 plus the additional interaction terms.
# Thus, Model 2 is the reduced model and Model 3 is the full model.
# A nested F-test will determine if the additional terms in the full model
# significantly add meaning to model.

## Partial F-test ##
partial_f_test <- anova(model_2, model_3)

# Print results
print(partial_f_test)

# Best predictor of Cholesterol -------------------------------------------
## Make Alcohol categorical ##
df_t$AlcoholConsumption <- ifelse(df_t$Alcohol == 0, 1,
                                  ifelse(df_t$Alcohol < 10, 2, 3)
                                  )

# Convert to factor with meaningful labels
df_t$AlcoholConsumption <- factor(df_t$AlcoholConsumption,
                                  levels = c(1, 2, 3),
                                  labels = c("None", "Some", "A lot"))

## Verify the categorization ##
cat("=== ALCOHOL CONSUMPTION Distribution ===\n")
table(df_t$AlcoholConsumption)

## Create dummy variables for regression analysis ##
# Reference category: None (AlcoholConsumption = 1)
df_t$AlcoholConsumption_Some <- ifelse(df_t$AlcoholConsumption == "Some", 1, 0)
df_t$AlcoholConsumption_ALot <- ifelse(df_t$AlcoholConsumption == "A lot", 1, 0)

## Descriptive statistics by groups ##
cat("\n=== Cholesterol by Smoking Status ===\n")
aggregate(Cholesterol ~ Smoke_Yes, data = df_t, 
          FUN = function(x) c(Mean = mean(x), SD = sd(x), N = length(x)))

cat("\n=== Cholesterol by Gender ===\n")
aggregate(Cholesterol ~ Gender_Male, data = df_t, 
          FUN = function(x) c(Mean = mean(x), SD = sd(x), N = length(x)))

cat("\n=== Cholesterol by Alcohol Consumption ===\n")
aggregate(Cholesterol ~ AlcoholConsumption, data = df_t, 
          FUN = function(x) c(Mean = mean(x), SD = sd(x), N = length(x)))

## Develop models ##
# Model 4A: Smoking Status + Fat (Parallel Slopes)
model_4a <- lm(Cholesterol ~ Smoke_Yes + Fat, data = df_t)
summary(model_4a)

# Model 4B: Smoking Status + Fat + Interaction (Unequal Slopes)
model_4b <- lm(Cholesterol ~ Smoke_Yes + Fat + Smoke_Yes:Fat, data = df_t)
summary(model_4b)

# Test for homogeneity of slopes (Smoking)
anova(model_4a, model_4b)

## MODEL 5A: Gender + Fat (Parallel Slopes) ##
model_5a <- lm(Cholesterol ~ Gender_Male + Fat, data = df_t)
summary(model_5a)

## MODEL 5B: Gender + Fat + Interaction (Unequal Slopes) ##
model_5b <- lm(Cholesterol ~ Gender_Male + Fat + Gender_Male:Fat, data = df_t)
summary(model_5b)

# Test for homogeneity of slopes (Gender)
anova(model_5a, model_5b)

## MODEL 6A: Alcohol Consumption + Fat (Parallel Slopes) ##
model_6a <- lm(Cholesterol ~ AlcoholConsumption_Some + AlcoholConsumption_ALot + Fat, data = df_t)
summary(model_6a)

## MODEL 6B: Alcohol Consumption + Fat + Interactions (Unequal Slopes) ##
model_6b <- lm(Cholesterol ~ AlcoholConsumption_Some + AlcoholConsumption_ALot + Fat + 
                 AlcoholConsumption_Some:Fat + AlcoholConsumption_ALot:Fat, data = df_t)
summary(model_6b)

# Test for homogeneity of slopes (Alcohol)
anova(model_6a, model_6b)

## MODEL 7A: Full Model with All Categorical Variables (Parallel Slopes) ##
model_7a <- lm(Cholesterol ~ Smoke_Yes + Gender_Male + AlcoholConsumption_Some + 
                 AlcoholConsumption_ALot + Fat, data = df_t)
summary(model_7a)

## MODEL 7B: Full Model with All Interactions (Unequal Slopes) ##
model_7b <- lm(Cholesterol ~ Smoke_Yes + Gender_Male + AlcoholConsumption_Some + 
                 AlcoholConsumption_ALot + Fat + 
                 Smoke_Yes:Fat + Gender_Male:Fat + 
                 AlcoholConsumption_Some:Fat + AlcoholConsumption_ALot:Fat, data = df_t)
summary(model_7b)

# Test for homogeneity of slopes (Full model)
anova(model_7a, model_7b)

## Compare all parallel slopes models ##
# Model comparison
cat("\n=== MODEL COMPARISON: R² VALUES ===\n")
cat(sprintf("Model 4A (Smoke + Fat): R² = %.4f\n", summary(model_4a)$r.squared))
cat(sprintf("Model 5A (Gender + Fat): R² = %.4f\n", summary(model_5a)$r.squared))
cat(sprintf("Model 6A (Alcohol + Fat): R² = %.4f\n", summary(model_6a)$r.squared))
cat(sprintf("Model 7A (All + Fat): R² = %.4f\n", summary(model_7a)$r.squared))

# AIC comparison
cat("\n=== AIC COMPARISON ===\n")
cat(sprintf("Model 4A AIC: %.2f\n", AIC(model_4a)))
cat(sprintf("Model 5A AIC: %.2f\n", AIC(model_5a)))
cat(sprintf("Model 6A AIC: %.2f\n", AIC(model_6a)))
cat(sprintf("Model 7A AIC: %.2f\n", AIC(model_7a)))
cat(sprintf("Model 7B AIC: %.2f\n", AIC(model_7b)))

## Scatterplots by categorical variables ##
# By Smoking Status
ggplot(df_t, aes(x = Fat, y = Cholesterol, color = factor(Smoke_Yes))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("0" = "#2E7D32", "1" = "#D32F2F"),
                     labels = c("Non-smoker", "Smoker")) +
  labs(title = "Cholesterol vs Fat by Smoking Status",
       x = "Fat", 
       y = "Cholesterol",
       color = "Smoking Status") +
  theme_bw()
ggsave("smoke_plot.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# By Gender
ggplot(df_t, aes(x = Fat, y = Cholesterol, color = factor(Gender_Male))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = c("0" = "#7B1FA2", "1" = "#1976D2"),
                     labels = c("Female", "Male")) +
  labs(title = "Cholesterol vs Fat by Gender",
       x = "Fat Intake", 
       y = "Cholesterol",
       color = "Gender") +
  theme_bw()
ggsave("gender_plot.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# By Alcohol Consumption
ggplot(df_t, aes(x = Fat, y = Cholesterol, color = AlcoholConsumption)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = c("None" = "#F57C00", 
                                "Some" = "#388E3C", 
                                "A lot" = "#1976D2")) +
  labs(title = "Cholesterol vs Fat Intake by Alcohol Consumption",
       x = "Fat Intake",
       y = "Cholesterol",
       color = "Alcohol\nConsumption") +
  theme_bw(base_size = 14)
ggsave("alcohol_plot.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)
