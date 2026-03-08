## ============================================================
## Assignment 9
## Stress Data Set
## Poisson & ZIP Model Regression on Stress Dataset
## Response Variable: STRESS
## ============================================================

# Set-up environment ------------------------------------------------------
# Clear environment
rm(list = ls())

# Load libraries
library(corrplot)
library(MASS)
library(tidyverse)
library(car)
library(readxl)
library(pscl)

# Import dataset
stress_data <- readxl::read_xlsx("STRESS.xlsx") %>%
  select(-NEWID)

# Create output directory
output_dir <- "Output"

if (!dir.exists(output_dir)) {
  dir.create(path = paste(dirname(getwd()), "/Output", sep = ""))
} else {
  print("Directory already exists!")
}
# Output path
output_path <- file.path(dirname(getwd()), "Output", sep = "")

# Stress Variable ---------------------------------------------------------
## Histogram plot ##
ggplot(stress_data, aes(x = factor(STRESS))) +
  geom_histogram(stat = "count", color = "black", fill = "steelblue") +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 6) +
  labs(
    title = "STRESS Histogram Distribution",
    x = "STRESS Score",
    y = "Frequency"
  ) +
  scale_y_continuous(expand = expansion(mult = c(.05,.12))) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("stress_histogram.png", path = output_path, units = "in",
       height = 6, width = 10, dpi = 300)

# INTERPRETATION: The STRESS variable is NOT normally distributed as most of the variables
# are 0, with very few variables in the far right tail, indicating some right skewness.

## Summary statistics ##
# Define function to calculate mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# Summary table
summary_table <- stress_data %>%
  select(STRESS) %>%
  summarise(
    n = n(),
    mean = mean(STRESS),
    median = median(STRESS),
    mode = find_mode(STRESS),
    var = var(STRESS),
    sd = sd(STRESS),
    min = min(STRESS),
    max = max(STRESS),
    range = sprintf("%d - %d", min(STRESS), max(STRESS))
  )
summary_table

# Interpretation: The mean > median, providing further evidence that there is 
# right skewness in the data

## Q-Q Plot ##
png(paste0(output_path, "/qq_plot.png"), units = "in", width = 10, height = 6,
    res = 300)
qqnorm(stress_data$STRESS, pch = 1, frame = FALSE)
qqline(stress_data$STRESS, col = "steelblue", lwd = 2)
dev.off()
qqnorm(stress_data$STRESS, pch = 1, frame = FALSE)
qqline(stress_data$STRESS, col = "steelblue", lwd = 2)

# INTERPRETATION: Similar to the above findings, there is further evidence that
# the distribution of STRESS is not normal and contains skewness.

# Modeling Framework -----------------------------------------------------
## Set seed for reproducibility ##
set.seed(123)

## Train/test split ##
stress_data$u <- runif(nrow(stress_data), 0, 1)
train.df <- subset(stress_data, u < 0.70) %>% select(-u)
test.df  <- subset(stress_data, u >= 0.70) %>% select(-u)

cat("Training set:", nrow(train.df), "| Test set:", nrow(test.df), "\n")

# OLS Regression ----------------------------------------------------------
## Fit model ##
OLS_model <- lm(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, data = stress_data)

# Summary of model
summary_OLS_model <- summary(OLS_model)
print(summary_OLS_model)

anova_OLS_model <- anova(OLS_model)
print(anova_OLS_model)

## Predicted vals vs standardized residuals plot ##
# Get predicted values
stress_data$y_hat_m1 <- fitted.values(OLS_model)

# Create plot
ggplot(data = stress_data, aes(x = y_hat_m1)) +
  geom_histogram(color = "black", fill = "salmon", binwidth = 0.1) +
  labs(title = "Predicted Values Histogram (OLS Regression)",
       x = "Predicted values",
       y = "Frequency") +
  theme_bw(base_size = 14)
ggsave("pred_vals_model_1.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# INTERPRETATION: The histogram of OLS predicted values likely displays a roughly
# bell-shaped distribution centered around the mean STRESS count. However, this
# distribution is problematic for count data for two key reasons: (1) OLS can
# generate negative predicted values, which are mathematically impossible for a
# count outcome like STRESS, and (2) the predicted distribution does not reflect
# the zero-inflated, right-skewed nature of the actual STRESS variable. These
# violations suggest that OLS is a poor model choice for this outcome and that
# count-specific models (Poisson, Negative Binomial, or ZIP) are more appropriate.

# Homoscedasticity test
ncvTest(OLS_model)

## Leverage, Influence, & Outliers ##
# Residual plots
png(paste0(output_path, "/residual_plots_m1.png"), width = 10, height = 6, units = "in", res = 300)
residualPlots(OLS_model, main = "Residual Plots (OLS Regression)")
dev.off()
residualPlots(OLS_model, main = "Residual Plots (OLS Regression)")

# Influence measures
influence.measures(OLS_model) # Influence measures

png(paste0(output_path, "/influence_index_plot_m1.png"), width = 12, height = 8, units = "in", res = 300)
influenceIndexPlot(OLS_model) # Influence index plot (leverage)
dev.off()
influenceIndexPlot(OLS_model)

png(paste0(output_path, "/influence_plot_m1.png"), width = 10, height = 6, units = "in", res = 300)
influencePlot(OLS_model) # Influence plot (leverage)
dev.off()
influencePlot(OLS_model)

# Outlier test
outlierTest(OLS_model)
outlier_results_OLS_model <- data.frame(
  rstudent = outlierTest(OLS_model)$rstudent,
  unadjusted_p = outlierTest(OLS_model)$p,
  bonferroni_p = outlierTest(OLS_model)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_OLS_model, file = paste0(output_path, "/outlier_results_table_m1.csv"), row.names = TRUE)

# Transformed OLS Regression ----------------------------------------------
## Fit model ##
ln_OLS_model <- lm(log1p(STRESS) ~ COHES + ESTEEM + GRADES + SATTACH, data = stress_data)

# Summary of model
summary_ln_OLS_model <- summary(ln_OLS_model)
print(summary_ln_OLS_model)

## Extract coefficients ##
coef_table_2 <- summary(ln_OLS_model)$coefficients
print(coef_table_2)

## Predicted values plot ##
# Get predicted values
stress_data$y_hat_m2 <- fitted.values(ln_OLS_model)

# Create plot
ggplot(data = stress_data, aes(x = y_hat_m2)) +
  geom_histogram(color = "black", fill = "#009999", binwidth = 0.05) +
  labs(title = "Predicted Values (LN OLS Regression)",
       x = "Predicted values",
       y = "Frequency") +
  theme_bw(base_size = 14)
ggsave("pred_vals_model_2.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# INTERPRETATION: Applying a log1p transformation (log(1 + STRESS)) before fitting
# OLS improves the distributional properties of the residuals and shifts predicted
# values toward a more symmetric, bell-shaped distribution in log-scale. This
# addresses the normality assumption more closely than the untransformed OLS model.
# However, back-transforming predictions to the original scale via expm1() will
# produce a log-normal correction bias unless a smearing factor is applied.
# Additionally, the log transformation does not fully resolve the structural
# zero-inflation issue — zeros are collapsed to log1p(0) = 0 rather than modeled
# as a separate generative process. Compare residual plots to the untransformed
# OLS model to confirm improvement in homoscedasticity., width = 10, height = 6, units = "in", res = 300)

## Leverage, Influence, & Outliers ##
# Residual plots
png(paste0(output_path, "/residual_plots_m2.png"), width = 10, height = 6, units = "in", res = 300)
residualPlots(ln_OLS_model, main = "Residual Plots (LN OLS Regression)")
dev.off()
residualPlots(ln_OLS_model, main = "Residual Plots (LN OLS Regression)")

# Influence measures
influence.measures(ln_OLS_model) # Influence measures

png(paste0(output_path, "/influence_index_plot_m2.png"), width = 12, height = 8, units = "in", res = 300)
influenceIndexPlot(ln_OLS_model) # Influence index plot (leverage)
dev.off()
influenceIndexPlot(ln_OLS_model)

png(paste0(output_path, "/influence_plot_m2.png"), width = 10, height = 6, units = "in", res = 300)
influencePlot(ln_OLS_model) # Influence plot (leverage)
dev.off()
influencePlot(ln_OLS_model)

# Outlier test
outlierTest(ln_OLS_model)
outlier_results_ln_OLS_model <- data.frame(
  rstudent = outlierTest(ln_OLS_model)$rstudent,
  unadjusted_p = outlierTest(ln_OLS_model)$p,
  bonferroni_p = outlierTest(ln_OLS_model)$bonf
)
# Save the data frame to a CSV file
write.csv(outlier_results_ln_OLS_model, file = paste0(output_path, "/outlier_results_table_m2.csv"), row.names = TRUE)

# Poisson Regression ------------------------------------------------------
## Fit model ##
poisson_model <- glm(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, family = poisson(link = "log"), data = stress_data)

# Summary of model
summary_poisson_model <- summary(poisson_model)
print(summary_poisson_model)

## Over dispersed Poisson ##
overdispersed_model <- glm.nb(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, data = stress_data)

# Summary of model
summary_overdispersed_model <- summary(overdispersed_model)
summary(overdispersed_model)

## Extract coefficients ##
coef_table_3 <- coef(poisson_model)
print(coef_table_3)

## Predicted Values ##
# Compute means and SDs
COHES_mean <- mean(stress_data$COHES)
COHES_sd <- sd(stress_data$COHES)
ESTEEM_mean <- mean(stress_data$ESTEEM)
GRADES_mean <- mean(stress_data$GRADES)
SATTACH_mean <- mean(stress_data$SATTACH)

# Define three COHES group values
cohes_low <- COHES_mean - COHES_sd
cohes_mid <- COHES_mean
cohes_high <- COHES_mean + COHES_sd

# Compute linear predictors manually for each group
lp_low <- coef_table_3["(Intercept)"] + coef_table_3["COHES"]*cohes_low  + coef_table_3["ESTEEM"]*ESTEEM_mean + 
  coef_table_3["GRADES"]*GRADES_mean + coef_table_3["SATTACH"]*SATTACH_mean

lp_middle <- coef_table_3["(Intercept)"] + coef_table_3["COHES"]*cohes_mid  + coef_table_3["ESTEEM"]*ESTEEM_mean + 
  coef_table_3["GRADES"]*GRADES_mean + coef_table_3["SATTACH"]*SATTACH_mean

lp_high <- coef_table_3["(Intercept)"] + coef_table_3["COHES"]*cohes_high + coef_table_3["ESTEEM"]*ESTEEM_mean + 
  coef_table_3["GRADES"]*GRADES_mean + coef_table_3["SATTACH"]*SATTACH_mean

# Apply the inverse link (exp) to get predicted counts
pred_low <- exp(lp_low)
pred_middle <- exp(lp_middle)
pred_high <- exp(lp_high)

# View results
manual_predictions <- c(Low = pred_low, Middle = pred_middle, High = pred_high)
print(manual_predictions)

## AIC & BIC ##
# Calculate AIC & BIC
AIC_poisson <- AIC(poisson_model)
AIC_overdispersed <- AIC(overdispersed_model)
BIC_poisson <- BIC(poisson_model)
BIC_overdispersed <- BIC(overdispersed_model)

# Convert to data frame
results <- data.frame(
  Model = c("Poisson Model", "Overdispersed Model"),
  AIC = c(AIC_poisson, AIC_overdispersed),
  BIC = c(BIC_poisson, BIC_overdispersed)
)

# Ranking (lower is better)
ranks <- data.frame(
  AIC_Rank = rank(results$AIC),
  BIC_Rank = rank(results$BIC)
)

final_table <- cbind(results, ranks)
final_table

## Deviance residuals by predicted values ##
# Get deviance residuals and predicted values
stress_data$poisson_dev_resids <- summary_poisson_model$deviance.resid
stress_data$y_hat_m3 <- fitted(poisson_model)

# Manual check
y_hat_hard <- coef_table_3["(Intercept)"] + coef_table_3["COHES"]*stress_data$COHES  + coef_table_3["ESTEEM"]*stress_data$ESTEEM + 
  coef_table_3["GRADES"]*stress_data$GRADES + coef_table_3["SATTACH"]*stress_data$SATTACH
check <- exp(y_hat_hard)

cat(sprintf("Number of observations in the dataset:                            %s", nrow(stress_data)))
cat(sprintf("Number of agreement between fitted function & manual calculation: %s", sum(stress_data$y_hat_m3 == check)))

# Plot
ggplot(data = stress_data, aes(x = poisson_dev_resids, y = y_hat_m3)) +
  geom_point() +
  labs(
    title = "Poisson Deviance Residuals vs Predicted Values (Poisson Models)",
    x = "Poisson Deviance Residuals",
    y = "Predicted Values"
  ) +
  theme_bw(base_size = 16)
ggsave("dev_resids_pred_vals_plot.png", width = 10, 
       height = 6, units = "in", dpi = 300, path = output_path)

# Logistic Regression -----------------------------------------------------
## Create new variable ##
stress_data$Y_IND <- ifelse(stress_data$STRESS == 0, 0, 1)

## Fit Logistic Model ##
# Full model
logistic_model <- glm(Y_IND ~ COHES + ESTEEM + GRADES + SATTACH, family = binomial(link = "logit"), data = stress_data)

# Null model
null_model <- glm(Y_IND ~ 1, family = binomial(link = "logit"), data = stress_data)

# Summary of model
summary_logistic <- summary(logistic_model)
print(summary_logistic)

## Extract coefficients ##
coef_table_4 <- coef(logistic_model)
print(coef_table_4)

## Goodness of fit ##
# AIC / BIC
cat("AIC:", round(AIC(logistic_model), 2), "\n")
cat("BIC:", round(BIC(logistic_model), 2), "\n")

# Likelihood ratio test vs null
cat("\nLikelihood Ratio Test (vs null model):\n")
anova(null_model, logistic_model, test = "Chisq")

# INTERPRETATION: The logistic regression model predicts Y_IND — whether any stressful
# event occurred at all (STRESS > 0 vs. STRESS = 0). This serves as the zero-inflation
# component of the manual ZIP model. Predictors with significant negative coefficients
# (e.g., COHES — family cohesion, ESTEEM — self-esteem) indicate that higher values
# of those constructs are associated with reduced odds of experiencing any stressful
# event. The likelihood ratio test against the null model confirms whether the predictors
# collectively improve model fit beyond chance; a significant p-value (< 0.05) supports
# retaining the predictors. AIC and BIC provide a baseline for comparing this logistic
# component against the full ZIP model fitted via pscl.
#
# RECOMMENDATION: Use the logistic model's predicted probabilities (prob_hatzip1) as
# the P(stress present) component in the manual ZIP calculation. Confirm that the
# logistic model is well-calibrated — check that the predicted probability distribution
# has a reasonable range and is not collapsed near 0 or 1 for most observations.
# If GRADES or SATTACH are not significant here, they are candidates for removal
# from the zero-inflation component when constructing the final pscl ZIP model.

# Manual ZIP Model --------------------------------------------------------
######### STEPS: 
######### 1. Use a logistic regression model to fit the Y_IND variable.
######### 2. Then use a Poisson regression to fit the number of stressful events (STRESS_IND)
######### 3. Finally, combine the two together to obtain the predicted values

## Fit Logistic model ##
zipfit1 <- glm(Y_IND ~ COHES + ESTEEM + GRADES + SATTACH, 
               family = binomial(link = "logit"), data = stress_data)
summary(zipfit1)

# Extract coefficients
coef_table_4 <- coef(zipfit1)
print(coef_table_4)

# Get predicted values for logistic model
y_hat_zip1 <- coef_table_4["(Intercept)"] + coef_table_4["COHES"]*stress_data$COHES  + coef_table_4["ESTEEM"]*stress_data$ESTEEM + 
  coef_table_4["GRADES"]*stress_data$GRADES + coef_table_4["SATTACH"]*stress_data$SATTACH
zipodds <- exp(y_hat_zip1)
prob_hatzip1<- zipodds / (1+zipodds)

## Fit Poisson model ##
# Create STRESS_IND variable if stress is present
stress_data <- stress_data %>%
  mutate(
    STRESS_IND = ifelse(Y_IND == 1, STRESS, NA)
  )
zipfit2 <- glm(STRESS_IND ~ COHES + ESTEEM + GRADES + SATTACH, 
               family = poisson(link = "log"), data = stress_data)
summary(zipfit2)

# Extract coefficients
coef_table_5 <- coef(zipfit2)
print(coef_table_5)

# Get predicted values for Poisson model
y_hat_zip2 <- coef_table_5["(Intercept)"] + coef_table_5["COHES"]*stress_data$COHES  + coef_table_5["ESTEEM"]*stress_data$ESTEEM + 
  coef_table_5["GRADES"]*stress_data$GRADES + coef_table_5["SATTACH"]*stress_data$SATTACH
lambda_hatzip2 <- exp(y_hat_zip2) # E[STRESS | stress present]

## Combine for final prediction values ##
# ZIP combined prediction
# E[Y] = P(present) * E[count | present]
stress_data$y_hat_m4 <- prob_hatzip1 * lambda_hatzip2

# Residuals
stress_data$zip_resid <- stress_data$STRESS - stress_data$y_hat_m4

## Basic fit diagnostics ##
cat("RMSE:", sqrt(mean(stress_data$zip_resid^2)), "\n")
cat("MAE:", mean(abs(stress_data$zip_resid)), "\n")
cat("Correlation (predicted vs actual):", 
    cor(stress_data$y_hat_m4, stress_data$STRESS), "\n")

# How often does the model correctly identify zeros?
# (using 0.5 threshold on predicted probability from logistic)
stress_data$predicted_zero <- ifelse(prob_hatzip1 < 0.5, 1, 0)
table(Predicted_Zero = stress_data$predicted_zero, 
      Actual_Zero = 1 - stress_data$Y_IND)

# Compare variance of residuals to variance of STRESS
cat("Var(STRESS):", var(stress_data$STRESS), "\n")
cat("Var(residuals):", var(stress_data$zip_resid), "\n")

# Compare predicted mean to actual mean
cat("Mean STRESS:", mean(stress_data$STRESS), "\n")
cat("Mean predicted:", mean(stress_data$y_hat_m4), "\n")

# pscl ZIP model ----------------------------------------------------------
###### STEPS:
###### 1. Fit model will same predictors
###### 2. Fit model with best fitting model

## Fit first model ##
# Model 1: Use all variables on both sides
pscl_zip1 <- zeroinfl(STRESS ~ COHES + ESTEEM + GRADES + SATTACH  | 
                        COHES + ESTEEM + GRADES + SATTACH, 
                      dist = "poisson", data = stress_data, EM = TRUE) 
summary(pscl_zip1)
stress_data$y_hat_m5 <- fitted(pscl_zip1)

## Fit best fitting model ##
### RATIONALE: Removed GRADES &  SATTACH as they were not significant in first model
pscl_zip2 <- zeroinfl(STRESS ~ COHES + ESTEEM | COHES,  data = stress_data, 
                      dist = "negbin", EM = TRUE)
summary(pscl_zip2)

# Comparing AIC and BIC
AIC(pscl_zip1, pscl_zip2)
BIC(pscl_zip1, pscl_zip2)

# Intercept-only zero-inflation
pscl_zip3 <- zeroinfl(STRESS ~ COHES + ESTEEM | 1, 
                      dist = "negbin", EM = TRUE, data = stress_data)
summary(pscl_zip3)

# Adding SATTACH back to count side
pscl_zip4 <- zeroinfl(STRESS ~ COHES + ESTEEM + SATTACH | 1, 
                      dist = "negbin", EM = TRUE, data = stress_data)
summary(pscl_zip4)

# Compare all models
AIC(pscl_zip1, pscl_zip2, pscl_zip3, pscl_zip4)
BIC(pscl_zip1, pscl_zip2, pscl_zip3, pscl_zip4)

# CONCLUSION: Choose model 3 based on BIC being the lowest. COHES and ESTEEM  have been 
# proven factors in adolescence stressful events.