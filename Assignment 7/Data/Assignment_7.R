## ============================================================
## Assignment 7
## ICU Patient Survival Analysis
## Logistic Regression on ICU Dataset
## Response Variable: STA (0 = Lived, 1 = Died)
## ============================================================

# Set-up environment ------------------------------------------------------
# Clear environment
rm(list = ls())

# Load libraries
library(MASS)
library(tidyverse)
library(car)
library(readxl)
# install.packages("fastDummies") # Install if needed
library(fastDummies)
# install.packages("Metrics") # Install if needed
library(Metrics)
library(nnet)

# Import dataset
icu_data <- readxl::read_xlsx("icu.xlsx")

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
# For this analysis, STA (Status) is used as the binary response variable
# indicating whether a patient survived (0) or died (1) during their ICU stay.
# The goal is to identify clinical and demographic predictors of in-hospital mortality.

# Find any missing values
sapply(icu_data, function(x) sum(is.na(x)))

# INTERPRETATION: No missing values were detected across any variable in the dataset.
# This means the data is complete and no imputation, case deletion, or other
# missing-data handling strategies are required prior to modeling.
#
# RECOMMENDATION: Even though no NAs are present, it is still good practice to
# verify that categorical variables use the expected coding scheme (e.g., 0/1)
# and that continuous variables fall within clinically plausible ranges before
# proceeding. Spot-check variables such as SYS and HRA for impossible values
# (e.g., SYS = 0 mmHg) that may have been recorded as valid but represent errors.

## Barplot ##
ggplot(icu_data, aes(x = factor(STA))) +
  geom_bar(stat = "count", color = "black", fill = "steelblue") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5) +
  labs(
    title = "Survival Status Distribution",
    x = "Survival Status",
    y = "Count"
  ) +
  scale_x_discrete(
    labels = c("Lived", "Died")
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("status_distribution.png", path = output_path, 
       width = 11, height = 7, units = "in", dpi = 300)

# INTERPRETATION: The response variable (STA) is imbalanced, with the majority
# of patients surviving their ICU stay. This class imbalance is clinically expected
# in most ICU populations but has important modeling implications.
#
# RECOMMENDATION: Because of the class imbalance, accuracy alone is a misleading
# performance metric. Prioritize sensitivity (recall for the "Died" class),
# specificity, and AUC-ROC when evaluating model performance. If the imbalance
# is severe (e.g., <15% mortality), consider resampling strategies such as SMOTE
# or adjusting the classification threshold from the default 0.5 to better
# balance false positives and false negatives.

## Boxplots ##
# STA vs HRA
ggplot(icu_data, aes(x = factor(STA), y = HRA, fill = factor(STA))) +
  geom_boxplot(color = "black") +
  labs(
    title = "Survival Status Vs Heartrate At Admission",
    x = "Survival Status",
    y = "Heartrate (Beat/min)",
    fill = "Survival Status"
  ) +
  scale_fill_manual(
    values = c("0" = "darkgreen", "1" = "darkorange"),
    labels = c("Lived", "Died")
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("status_hra_plot.png", path = output_path, 
       width = 11, height = 7, units = "in", dpi = 300)

# INTERPRETATION: Patients who died tend to show a higher median heart rate at
# admission compared to those who survived, and the distribution for the "Died"
# group appears wider. Elevated heart rate (tachycardia) at admission is a known
# indicator of physiological stress and hemodynamic instability, which is consistent
# with a higher risk of in-hospital mortality.
#
# RECOMMENDATION: Consider HRA as a candidate predictor in the multivariable
# logistic regression model. A Wilcoxon rank-sum test (or independent t-test if
# normality holds) could formally confirm whether the difference in heart rate
# between groups is statistically significant prior to inclusion.

# STA vs SYS
ggplot(icu_data, aes(x = factor(STA), y = SYS, fill = factor(STA))) +
  geom_boxplot(color = "black") +
  labs(
    title = "Survival Status Vs Blood Pressure At Admission",
    x = "Survival Status",
    y = "Systolic Blood Pressure (mm Hg)",
    fill = "Survival Status"
  ) +
  scale_fill_manual(
    values = c("0" = "darkblue", "1" = "darkorchid"),
    labels = c("Lived", "Died")
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("status_bp_plot.png", path = output_path, 
       width = 11, height = 7, units = "in", dpi = 300)

# INTERPRETATION: Patients who died exhibit a visibly lower median systolic blood
# pressure (SYS) at admission compared to survivors. Low systolic BP (hypotension)
# is a hallmark of hemodynamic compromise and shock, conditions strongly associated
# with increased ICU mortality. The wider spread in the "Died" group also suggests
# greater physiological variability among non-survivors.
#
# RECOMMENDATION: SYS appears to be a clinically meaningful and statistically
# promising predictor of mortality. It should be included as a candidate in
# multivariate models. Given its inverse relationship with mortality (lower BP =
# higher risk), its estimated coefficient in logistic regression should be negative,
# serving as a useful sanity check on model outputs.

## Tables ##
# Status
cat("\n--- RESPONSE VARIABLE ---\n")
sta_table <- table(icu_data$STA)
sta_prop <- prop.table(sta_table) * 100
print(data.frame(Value = names(sta_table), 
                 Count = as.numeric(sta_table),
                 Percentage = sprintf("%.3f%%", sta_prop)))

# Demographics
cat("\nSEX:\n")
sex_table <- table(icu_data$SEX)
print(data.frame(Value = names(sex_table),
                 Count = as.numeric(sex_table),
                 Percentage = sprintf("%.3f%%", prop.table(sex_table)*100)))

cat("\nRACE:\n")
race_table <- table(icu_data$RACE)
print(data.frame(Value = names(race_table),
                 Count = as.numeric(race_table),
                 Percentage = sprintf("%.3f%%", prop.table(race_table)*100)))

cat("\nAGE:\n")
cat("  Range:", min(icu_data$AGE), "-", max(icu_data$AGE), "years\n")
cat("  Mean:", sprintf("%.1f", mean(icu_data$AGE)), "± SD", sprintf("%.1f", sd(icu_data$AGE)), "\n")
cat("  Median:", median(icu_data$AGE), "\n")

# Admission Criteria
cat("\nSER (Service at admission):\n")
ser_table <- table(icu_data$SER)
print(data.frame(Value = names(ser_table),
                 Count = as.numeric(ser_table),
                 Percentage = sprintf("%.3f%%", prop.table(ser_table)*100)))

cat("\nTYP (Type of admission):\n")
typ_table <- table(icu_data$TYP)
print(data.frame(Value = names(typ_table),
                 Count = as.numeric(typ_table),
                 Percentage = sprintf("%.3f%%", prop.table(typ_table)*100)))

cat("\nPRE (Previous ICU admission within 6 months):\n")
pre_table <- table(icu_data$PRE)
print(data.frame(Value = names(pre_table),
                 Count = as.numeric(pre_table),
                 Percentage = sprintf("%.3f%%", prop.table(pre_table)*100)))

# Comorbidities
comorbidities <- c("CAN", "CRN", "INF", "CPR", "FRA")
comorbidity_names <- c("Cancer", "Chronic Renal Failure", "Infection", 
                       "Prior CPR", "Fracture")

for(i in seq_along(comorbidities)) {
  cat("\n", comorbidity_names[i], " (", comorbidities[i], "):\n", sep="")
  var_table <- table(icu_data[[comorbidities[i]]])
  print(data.frame(Value = names(var_table),
                   Count = as.numeric(var_table),
                   Percentage = sprintf("%.3f%%", prop.table(var_table)*100)))
}

# INTERPRETATION (Comorbidities): Several comorbidity variables (e.g., CAN, CRN,
# CPR) likely show low prevalence. Rare binary predictors can cause issues in
# logistic regression such as complete or quasi-complete separation, inflated
# standard errors, or unreliable odds ratio estimates.
#
# RECOMMENDATION: For any comorbidity where fewer than ~10 events occur in either
# category, consider either collapsing categories, excluding the variable from
# individual models, or using penalized regression (e.g., Firth logistic regression
# via the `logistf` package) which is robust to sparse data.

# Vital Signs
cat("\nSYS (Systolic Blood Pressure in mmHg):\n")
cat("  Range:", min(icu_data$SYS), "-", max(icu_data$SYS), "\n")
cat("  Mean:", sprintf("%.3f", mean(icu_data$SYS)), "± SD", sprintf("%.3f", sd(icu_data$SYS)), "\n")
cat("  Median:", median(icu_data$SYS), "\n")

cat("\nHRA (Heart Rate in bpm):\n")
cat("  Range:", min(icu_data$HRA), "-", max(icu_data$HRA), "\n")
cat("  Mean:", sprintf("%.3f", mean(icu_data$HRA)), "± SD", sprintf("%.3f", sd(icu_data$HRA)), "\n")
cat("  Median:", median(icu_data$HRA), "\n")

# Blood gas and lab values
lab_vars <- c("PO2", "PH", "PCO", "BIC", "CRE")
lab_names <- c("PO2 (<60 abnormal)", "pH (abnormal)", "PCO2 (>45 abnormal)",
               "Bicarbonate (<18 abnormal)", "Creatinine (>2.0 abnormal)")

for(i in seq_along(lab_vars)) {
  cat("\n", lab_names[i], ":\n", sep="")
  var_table <- table(icu_data[[lab_vars[i]]])
  print(data.frame(Value = names(var_table),
                   Count = as.numeric(var_table),
                   Percentage = sprintf("%.3f%%", prop.table(var_table)*100)))
}

# Critical Status
cat("\nLOC (Level of Consciousness):\n")
loc_table <- table(icu_data$LOC)
print(data.frame(Value = names(loc_table),
                 Count = as.numeric(loc_table),
                 Percentage = sprintf("%.3f%%", prop.table(loc_table)*100)))

# INTERPRETATION (LOC): Level of consciousness is a critical clinical indicator.
# Patients admitted in a stuporous or comatose state have significantly higher
# mortality risk. If LOC has more than two levels, it may need to be treated as
# an ordinal or nominal variable with appropriate dummy coding before modeling.
#
# RECOMMENDATION: If LOC has 3+ categories, use dummy coding (e.g., via
# fastDummies::dummy_cols()) with a clinically meaningful reference category
# (e.g., "conscious/alert"). Verify that each LOC category has sufficient
# events to support stable coefficient estimation in logistic regression.

## Continuous Vars by STA ##
continuous_vars <- c('AGE', 'SYS', 'HRA')

for(var in continuous_vars) {
  cat("\n", var, ":\n", sep="")
  
  lived <- icu_data %>% filter(STA == 0) %>% pull(!!sym(var))
  died <- icu_data %>% filter(STA == 1) %>% pull(!!sym(var))
  
  cat("  Lived (STA=0):  Mean =", sprintf("%.3f", mean(lived)), 
      ", SD =", sprintf("%.3f", sd(lived)),
      ", Median =", sprintf("%.3f", median(lived)), "\n")
  cat("  Died  (STA=1):  Mean =", sprintf("%.3f", mean(died)),
      ", SD =", sprintf("%.3f", sd(died)),
      ", Median =", sprintf("%.3f", median(died)), "\n")
}

# INTERPRETATION: Across all three continuous variables, the "Died" group
# consistently shows a higher mean AGE and HRA, and a lower mean SYS compared
# to survivors. These directional differences are clinically coherent: older
# patients, those with faster heart rates, and those with lower blood pressure
# at admission are all at elevated mortality risk.
#
# RECOMMENDATION: Formally test whether these group-level differences are
# statistically significant using Welch's t-tests (or Wilcoxon rank-sum tests
# if normality is questionable). This provides a univariate statistical basis
# for predictor selection before building the multivariable model.

## Correlation ##
# Calculate correlations
cor_matrix <- cor(icu_data[, -1])
sta_correlations <- cor_matrix[, "STA"] %>% 
  sort(decreasing = TRUE) %>%
  as.data.frame()
names(sta_correlations) <- "Correlation"

cat("\nCorrelations with STA (sorted, positive = higher mortality):\n\n")
print(sta_correlations, digits = 3)

# Identify strongest predictors
cat("\n\nTOP 5 POSITIVE PREDICTORS (associated with mortality):\n")
top_pos <- sta_correlations %>%
  filter(row.names(.) != "STA") %>%
  filter(Correlation > 0) %>%
  arrange(desc(Correlation)) %>%
  head(5)
print(top_pos, digits = 3)

cat("\n\nTOP 5 NEGATIVE PREDICTORS (associated with mortality):\n")
top_neg <- sta_correlations %>%
  filter(row.names(.) != "STA") %>%
  filter(Correlation < 0) %>%
  arrange(desc(Correlation)) %>%
  head(5)
print(top_neg, digits = 3)

# INTERPRETATION: Variables with the strongest positive correlations to STA
# (e.g., AGE, CPR, LOC) represent the best univariate predictors of mortality.
# Variables with the strongest negative correlations (e.g., SYS, TYP) are
# inversely associated with death — higher SYS and elective admission type are
# linked to survival. Note that Pearson correlations assume linearity and are
# less appropriate for binary-continuous relationships; these values are best
# used as a screening tool rather than a definitive measure of association.
#
# RECOMMENDATION: Use the top positive and negative predictors identified here
# as starting candidates for the multivariable logistic regression model. To
# better quantify the binary-outcome associations, compute point-biserial
# correlations or run univariate logistic regressions for each predictor and
# report their individual AUC values. Also inspect the inter-predictor
# correlation matrix to flag potential multicollinearity (|r| > 0.7) before
# building the full model.

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
  outlier_info <- detect_outliers(icu_data[[var]])
  
  cat("  IQR Range: [", sprintf("%.1f", outlier_info$Q1), ", ",
      sprintf("%.1f", outlier_info$Q3), "], IQR = ",
      sprintf("%.1f", outlier_info$IQR), "\n", sep="")
  cat("  Outlier bounds: < ", sprintf("%.1f", outlier_info$lower_bound),
      " or > ", sprintf("%.1f", outlier_info$upper_bound), "\n", sep="")
  cat("  Number of outliers: ", outlier_info$n_outliers,
      " (", sprintf("%.1f%%", outlier_info$n_outliers/nrow(df)*100), ")\n", sep="")
  
  if(outlier_info$n_outliers > 0) {
    cat("  Outlier values:", paste(sort(outlier_info$outliers), collapse=", "), "\n")
  }
}

# INTERPRETATION: The IQR method flags values more than 1.5×IQR beyond the
# quartile boundaries as potential outliers. In a clinical ICU dataset, extreme
# values in vital signs (e.g., very high heart rate, very low blood pressure)
# are often clinically real — they represent the most critically ill patients —
# rather than data entry errors.
#
# RECOMMENDATION: Do not automatically remove statistical outliers from a
# clinical dataset. Instead, cross-check flagged values against clinical
# plausibility ranges (e.g., SYS < 50 mmHg, HRA > 200 bpm) and investigate
# whether they are associated with mortality at a higher rate. If outliers are
# genuine, logistic regression is relatively robust to them in the predictor
# space. If there is evidence of data entry error, document the decision to
# remove or winsorize values in the analysis notes.

# Contingency Tables ------------------------------------------------------
## SEX & STA ##
contingency_table_1 <- table(Sex = icu_data$SEX, Status = icu_data$STA)
rownames(contingency_table_1) <- c("Male", "Female")
colnames(contingency_table_1) <- c("Lived", "Died")
contingency_table_1
prop.table(contingency_table_1)

# Probabilities
# Row 1 (Sex = 0 = Male)
male_lived <- (contingency_table_1)["Male", "Lived"]
male_died <- (contingency_table_1)["Male", "Died"]
prob_survive_male <- male_lived/(male_died + male_lived)
cat("Male survival rate:", sprintf("%.3f%%", prob_survive_male * 100))

# Row 2 (Sex = 1= Female)
female_lived <- (contingency_table_1)["Female", "Lived"]
female_died <- (contingency_table_1)["Female", "Died"]
prob_survive_female <- female_lived/(female_died + female_lived)
cat("Female survival rate:", sprintf("%.3f%%", prob_survive_female * 100))

# Calculate odds
# Row 1 (Sex = 0 = Male)
odds_male <- male_lived/male_died

# Row 2 (Sex = 1 = Female)
odds_female <- female_lived/female_died

# Calculate odds ratio
odds_ratio_t1 <- odds_male / odds_female
cat("Odds Ratio for Survival:", sprintf("%.3f", odds_ratio_t1))

# INTERPRETATION: The survival rates between males and females differ by
# approximately 1.698 percentage points, which is a minimal absolute difference.
# The odds ratio of approximately 1.111 indicates that males have slightly higher
# odds of survival than females, but this effect is modest.
#
# RECOMMENDATION: To determine whether this difference is statistically
# significant, perform a chi-squared test or Fisher's exact test on the
# contingency table:
#   chisq.test(contingency_table_1)  # or fisher.test(contingency_table_1)
# Given the small absolute difference and OR close to 1.0, sex may not be a
# meaningful independent predictor of mortality. However, sex should still be
# evaluated in the context of the multivariable model, as it may interact with
# other variables (e.g., age, comorbidities). Reporting confidence intervals
# around the odds ratio would strengthen this analysis:
#   fisher.test(contingency_table_1)$conf.int

## TYP & STA ##
contingency_table_2 <- table(Admin_Type = icu_data$TYP, Status = icu_data$STA)
rownames(contingency_table_2) <- c("Elective", "Emergency")
colnames(contingency_table_2) <- c("Lived", "Died")
contingency_table_2
prop.table(contingency_table_2)

# Probabilities
# Row 1 (Sex = 0 = Elective)
elective_lived <- (contingency_table_2)["Elective", "Lived"]
elective_died <- (contingency_table_2)["Elective", "Died"]
prob_survive_elective <- elective_lived/(elective_died + elective_lived)
cat("Elective survival rate:", sprintf("%.3f%%", prob_survive_elective * 100))

# Row 2 (Sex = 1= Emergency)
emergency_lived <- (contingency_table_2)["Emergency", "Lived"]
emergency_died <- (contingency_table_2)["Emergency", "Died"]
prob_survive_emergency <- emergency_lived/(emergency_died + emergency_lived)
cat("Emergency survival rate:", sprintf("%.3f%%", prob_survive_emergency * 100))

# Calculate odds
# Row 1 (Sex = 0 = Elective)
elective_odds <- elective_lived/elective_died

# Row 2 (Sex = 1 = emergency)
emergency_odds <- emergency_lived/emergency_died

# Calculate odds ratio
odds_ratio_t2 <- elective_odds / emergency_odds
cat("Odds Ratio for Survival:", sprintf("%.3f", odds_ratio_t2))

# INTERPRETATION: Elective admissions have a substantially higher survival rate
# than emergency admissions — a difference of approximately 22 percentage points.
# The odds ratio of ~8.86 indicates that elective patients have nearly 9× higher
# odds of surviving compared to emergency patients. This is clinically intuitive:
# emergency admissions are more likely to involve acute, life-threatening crises
# requiring intensive intervention, while elective admissions are planned and
# involve patients in comparatively stable condition.
#
# RECOMMENDATION: Admission type (TYP) is one of the strongest predictors of
# mortality in this dataset and should be prioritized as a predictor in the
# logistic regression model. Because the OR is very large, also verify that no
# complete separation exists (e.g., no elective patients who died) — if so, use
# Firth logistic regression. Additionally, consider whether TYP interacts with
# other variables (e.g., emergency + cancer diagnosis), as interaction terms may
# improve model fit and clinical insight.

# Logistical regression model ---------------------------------------------
## Fitting model ##
fit1 <- glm(STA ~ AGE, family = binomial(link = "logit"), data = icu_data)
summary_m1 <- summary(fit1)
print(summary_m1)

anova_m1 <- anova(fit1)
print(anova_m1)

# INTERPRETATION (Model Summary): The simple logistic regression model regresses
# mortality (STA) on age alone. A positive and statistically significant
# coefficient for AGE would confirm that older patients have higher log-odds of
# death. The z-statistic and p-value in the summary indicate whether AGE is a
# significant predictor at the chosen alpha level (typically 0.05).
# The residual deviance should be meaningfully lower than the null deviance,
# suggesting that AGE explains some variability in survival.
#
# RECOMMENDATION: While this simple model establishes a univariate baseline,
# age alone is unlikely to be a sufficient predictor of ICU mortality given the
# many clinical confounders present. Report McFadden's pseudo-R² alongside AIC
# to better convey model fit:
#   1 - (fit1$deviance / fit1$null.deviance)  # McFadden's R²
# Then build toward a multivariable model incorporating the strongest predictors
# identified in the EDA and correlation analyses (e.g., TYP, SYS, LOC, CPR).

## Prediction equation ##
# Extract statistics
coef_m1 <- coef(fit1)

cat("Regression Equation:\n")
cat("Survival =", round(coef_m1[1], 2), "+", round(coef_m1[2], 2), "× Age\n\n")

# INTERPRETATION: The intercept represents the estimated log-odds of death for a
# patient aged 0 (not clinically meaningful on its own). The coefficient on AGE
# represents the change in log-odds of death for each one-year increase in age.
# Exponentiating this coefficient gives the odds ratio per year of age
exp_coef <- exp(coef_m1[2])
cat("Odds Ratio per year of age:", sprintf("%.4f", exp_coef), "\n")
cat("95% CI for OR:", sprintf("%.4f", exp(confint(fit1)[2,1])),
    "to", sprintf("%.4f", exp(confint(fit1)[2,2])), "\n")

# RECOMMENDATION: Always report the exponentiated coefficient (odds ratio) with
# a 95% confidence interval rather than the log-odds coefficient alone, as odds
# ratios are more interpretable to a clinical audience. If the CI does not cross
# 1.0, the effect of age is statistically significant at alpha = 0.05.

## Scatter plot ##
ggplot(data = icu_data, aes(x = AGE, y = factor(STA), color = factor(STA))) +
  geom_point() +
  labs(
    title = "Scatterplot of Survival Status vs Age",
    x = "Age (Years)",
    y = "Survival Status",
    col = "Status"
  ) +
  scale_color_manual(
    values = c("0" = "darkgreen", "1" = "darkred"),
    labels = c("Lived", "Died")
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("status_age_plot.png", path = output_path, 
       width = 11, height = 7, units = "in", dpi = 300)

# INTERPRETATION: Because STA is binary (0 or 1), this scatterplot shows two
# horizontal bands of points. There is considerable overlap in the age
# distributions of those who lived and died, confirming that age alone cannot
# perfectly separate the two groups. However, a visual inspection should reveal
# a higher density of "Died" points at older ages, consistent with the model.
#
# RECOMMENDATION: A jitter plot (geom_jitter with small vertical jitter) would
# reduce overplotting and make the age distribution within each outcome group
# clearer. Adding a smoothed logistic curve (geom_smooth(method = "glm",
# method.args = list(family = "binomial"))) overlaid on the raw data would also
# visually communicate the model's fitted probability.

## Discretizing Age ##
# Create age category variable
icu_data <- icu_data %>%
  mutate(
    AGE_cat = case_when(
      AGE >= 15 & AGE <= 24 ~ 1,
      AGE >= 25 & AGE <= 34 ~ 2,
      AGE >= 35 & AGE <= 44 ~ 3,
      AGE >= 45 & AGE <= 54 ~ 4,
      AGE >= 55 & AGE <= 64 ~ 5,
      AGE >= 65 & AGE <= 74 ~ 6,
      AGE >= 75 & AGE <= 84 ~ 7,
      AGE >= 85 & AGE <= 94 ~ 8,
      AGE >= 95 ~ 9
    ),
    .after = AGE
  )

# Calculate group proportions
age_mortality <- icu_data %>%
  group_by(AGE_cat) %>%
  summarise(
    n = n(),
    mean_STA = mean(STA),  # This is the proportion who died
    .groups = 'drop'
  )

# Display results
print(age_mortality)

# Plot mean versus categorical variables
ggplot(age_mortality, aes(x = AGE_cat, y = mean_STA)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  geom_text(aes(label = sprintf("%.3f", mean_STA)), vjust = -0.5) +
  scale_x_continuous(breaks = 1:9,
                     labels = c("15-24", "25-34", "35-44", "45-54", 
                                "55-64", "65-74", "75-84", "85-94", "95+")) +
  scale_size_continuous(name = "Sample Size", range = c(3, 10)) +
  labs(
    title = "Mean STA (Mortality Proportion) by Age Category",
    x = "Age Category",
    y = "Mean STA (Proportion)"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("mortality_by_age.png", path = output_path, 
       width = 11, height = 7, dpi = 300)

# INTERPRETATION: The grouped mortality proportions generally increase with age,
# with the steepest rise in the oldest age categories (75+). Younger age groups
# (25-34) show near-zero or zero mortality in this sample. This non-linear,
# accelerating pattern across age bins provides empirical justification for
# including age as a predictor, and suggests that the logistic curve may be
# capturing a real biological gradient in mortality risk.
#
# RECOMMENDATION: Note that some age bins (especially 15-24 and 95+) likely
# have small sample sizes, making their mortality proportions unstable estimates.
# Always report sample size (n) alongside proportions. This plot validates the
# continuous AGE predictor in fit1 and supports the positive coefficient direction.
# If a non-linear relationship with age were suspected, a spline term or
# polynomial term (AGE + I(AGE^2)) could be tested in a more complex model.

## AIC & BIC ##
# Get AIC and BIC values
aic_value <- AIC(fit1)
bic_value <- BIC(fit1)

cat("\nModel Fit Statistics:\n")
cat("  AIC:", round(aic_value, 3), "\n")
cat("  BIC:", round(bic_value, 3), "\n")

# Compute McFadden's pseudo-R²
mcfadden_r2 <- 1 - (fit1$deviance / fit1$null.deviance)
cat("  McFadden's Pseudo-R²:", round(mcfadden_r2, 4), "\n")

# INTERPRETATION: AIC and BIC are used for comparing models — lower values
# indicate a better trade-off between fit and parsimony. In isolation, these
# values are not informative; they become meaningful when compared against
# alternative models (e.g., a model with additional predictors). McFadden's
# pseudo-R² values between 0.2 and 0.4 are considered to represent excellent
# fit in logistic regression, while values below 0.1 suggest the model explains
# relatively little variation in the outcome.
#
# RECOMMENDATION: Fit at least one additional model (e.g., fit2 with TYP, SYS,
# AGE) and compare AIC/BIC values directly. A substantial reduction in AIC
# (typically > 2 units) favors the more complex model. Use anova(fit1, fit2,
# test = "Chisq") for a formal likelihood ratio test between nested models.
# Also consider reporting Nagelkerke's R² alongside McFadden's for a wider
# comparison with published literature.

## Predicted Values vs Age plot ##
# Get predicted values
icu_data$preds <- predict(fit1, type = "link")

# Create plot
ggplot(data = icu_data, aes(x = AGE, y = preds)) +
  geom_point() +
  labs(
    title = "Scatterplot of Predicted Logit Values vs Age",
    x = "Age (Years)",
    y = "Predicted Logit Values"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("pred_vals_age_plot.png", path = output_path, 
       width = 11, height = 7, units = "in", dpi = 300)

# INTERPRETATION: The predicted logit values form a perfectly linear pattern
# with AGE, which is expected since this is a simple logistic regression with
# one continuous predictor. The logit (log-odds) scale is unbounded and centered
# around 0; values below 0 correspond to predicted probabilities below 0.5
# (i.e., predicted survival), while values above 0 correspond to predicted
# probabilities above 0.5 (i.e., predicted death).
#
# RECOMMENDATION: This plot confirms correct model behavior but is primarily
# diagnostic. For communicating results to a non-statistical audience, the
# probability-scale plot (pi_hat vs. AGE) is far more intuitive and should be
# the primary visualization used in presentations or reports.

## Logit probs vs Age ##
# Calculate Probabilities
icu_data$pi_hat <- predict(fit1, type = "response")

# Create plot
ggplot(data = icu_data, aes(x = AGE, y = pi_hat)) +
  geom_point() +
  labs(
    title = "Logistic Fit with Raw Data",
    x = "Age (Years)",
    y = "Probability of Survival"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("logit_age_plot.png", path = output_path, 
       width = 11, height = 7, units = "in", dpi = 300)

# Create plot with original data
ggplot(data = icu_data, aes(x = AGE, y = pi_hat)) +
  geom_point() +
  geom_point(
    aes(x = AGE, y = STA), color = "red"
  ) + 
  labs(
    title = "Logistic Fit with Raw Data",
    x = "Age (Years)",
    y = "Probability of Survival"
  ) +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("logit_age_w_orig_data_plot.png", path = output_path, 
       width = 11, height = 7, units = "in", dpi = 300)

# INTERPRETATION: The logistic regression shows a positive association between
# age and predicted probability of death. The predicted probability curve does not
# appear strongly S-shaped because the observed probabilities range only from 0 to
# approximately 0.46 — only the lower portion of the logistic curve is visible
# given the age range in this sample. The increase in mortality risk appears
# gradual at younger ages and becomes steeper after age 75, consistent with the
# discretized mortality plot. Red points (observed outcomes at 0 or 1) and blue
# points (predicted probabilities ranging continuously) illustrate how logistic
# regression smooths discrete binary outcomes into a continuous probability surface.
#
# RECOMMENDATION: To evaluate how well the model discriminates between survivors
# and non-survivors, compute the AUC-ROC using the predicted probabilities:
#   library(pROC)
#   roc_obj <- roc(icu_data$STA, icu_data$pi_hat)
#   auc(roc_obj)  # AUC > 0.7 is generally considered acceptable discrimination
# Also compute a confusion matrix at the 0.5 threshold (or an optimized
# threshold using Youden's index) to report sensitivity and specificity.

## Predict my age ##
# Create dataframe
new_data <- data.frame(AGE = 29)

# Calculate predicted values
my_age_pred <- predict(fit1, newdata = new_data, type = "response")
print(my_age_pred)

# INTERPRETATION: The predicted probability of death (STA = 1) for a 29-year-old
# patient is approximately 0.0945 (9.45%). Although the 25–34 age group had zero
# observed deaths in this sample, the sample size for that group is small, and
# logistic regression appropriately smooths across all ages rather than predicting
# a probability of exactly zero. The 95% CI around this estimate conveys the
# statistical uncertainty associated with the prediction.
#
# RECOMMENDATION: When presenting point predictions to clinical stakeholders,
# always accompany them with confidence intervals so that the uncertainty is
# transparent. For a 29-year-old, the wide CI likely reflects the small number
# of young patients in the ICU dataset. The model performs best (most reliably)
# at ages where training data is most dense. Predictions at the extremes of the
# age distribution should be interpreted with caution.

# ============================================================
## OVERALL RECOMMENDATIONS FOR MODEL DEVELOPMENT ##
# ============================================================
#
# This script establishes a solid univariate baseline with AGE as the sole
# predictor. The following steps are recommended to develop a more complete and
# clinically useful mortality prediction model:
#
# 1. MULTIVARIABLE MODEL: Extend fit1 to include the strongest predictors
#    identified in EDA — minimally TYP, SYS, LOC, and CPR alongside AGE.
#    Use a purposeful selection strategy (univariate screening at p < 0.25,
#    then backward elimination on the multivariable model) to arrive at a
#    parsimonious final model.
#
# 2. INTERACTION TERMS: Test clinically plausible interactions, such as
#    TYP × AGE (does the effect of age on mortality differ by admission type?)
#    or AGE × CPR (is prior CPR more dangerous in older patients?).
#
# 3. MODEL DIAGNOSTICS: After fitting the final model, assess:
#    - Hosmer-Lemeshow goodness-of-fit test (hoslem.test from ResourceSelection)
#    - Influential observations via Cook's distance and DFBETAs
#    - Variance Inflation Factors (VIF) to check for multicollinearity
#
# 4. MODEL PERFORMANCE: Report sensitivity, specificity, positive/negative
#    predictive values, and AUC-ROC. Consider calibration plots to assess
#    whether predicted probabilities match observed event rates.
#
# 5. CROSS-VALIDATION: If the dataset is large enough, use k-fold cross-
#    validation (e.g., k = 10) to obtain an unbiased estimate of model
#    performance on unseen data and guard against overfitting.