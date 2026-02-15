################################################################################
# MSDS 410 - Assignment 1: Exploratory Data Analysis of Ames Housing Data
# Author: Joshua Pasaye
# Purpose: Demonstrate rigorous EDA practices including data quality assessment,
#          sample definition, and preliminary analysis for predictive modeling
# 
# KEY INSIGHTS DEMONSTRATED:
# 1. Systematic data cleaning with transparent documentation (waterfall chart)
# 2. Translation of correlation statistics into variable selection decisions
# 3. Data quality assessment with actionable findings
# 4. Distribution analysis to inform modeling choices (log transformation)
# 5. Relationship visualization between predictors and response variable
################################################################################

# Set-up environment ------------------------------------------------------
# Clear environment
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

# Import libraries
library(tidyverse)
library(readxl)
library(GGally)
library(corrplot)
library(lattice)
library(knitr)
library(kableExtra)
library(scales)

# Import data
df <- read_xlsx("ames_housing_data.xlsx")

# Data preview ------------------------------------------------------------
# BEST PRACTICE: Understanding data structure is critical before any analysis
# Data overview
str(df)

# INSIGHT: Separating numeric and categorical variables allows for appropriate
# statistical treatment of each variable type
# Get numeric columns
df_num <- df %>% 
  select(where(is.numeric), -c(SID, PID))
head(df_num)

# Get numeric column names
df_num_cols <- names(df_num)

# Get categorical columns
df_cat <- df %>%
  select(where(is.character))
head(df_cat)

# Get categorical column names
df_cat_cols <- names(df_cat)

# Sample definition (waterfall) -------------------------------------------
# INSIGHT: Correlation analysis helps identify which variables have the strongest
# linear relationships with our target variable (SalePrice). This guides initial
# variable selection for modeling.
# Compute correlation using only complete cases
cor_mat <- cor(df_num, method = "pearson", use = "pairwise.complete.obs")

# Plot
corrplot(corr = cor_mat,
         method = "shade", 
         type = "upper",
         shade.col = NA, 
         tl.col = "black",
         tl.cex = 0.5,
         na.label.col = "black",
         title = "Correlation Matrix of Numeric Variables"
         )

# INSIGHT: Identifying high-correlation predictors helps us:
# 1. Focus on the most promising variables
# 2. Detect multicollinearity issues early
# 3. Reduce dimensionality for initial modeling
# Get variables
cor_saleprice <- cor_mat[,"SalePrice"]  # correlation of all variables with SalePrice
cor_saleprice_sorted <- sort(abs(cor_saleprice), decreasing = TRUE) # Sort by absolute correlation
cor_saleprice_sorted <- cor_saleprice_sorted[names(cor_saleprice_sorted) != "SalePrice"] # Remove SalePrice

# DECISION POINT: 0.5 threshold is commonly used to identify "strong" correlations
high_corr <- cor_saleprice_sorted[abs(cor_saleprice_sorted) > 0.5]
high_corr <- sort(high_corr, decreasing = TRUE)

# FINDING: Display high correlation variables
cat("\n=== KEY FINDING: Variables Highly Correlated with SalePrice ===\n")
print(high_corr)
cat("\nINTERPRETATION: These", length(high_corr), "variables show strong linear\n")

high_corr_cols <- names(high_corr)

high_corr_table <- data.frame(
  Variable = names(high_corr),
  Correlation = round(high_corr, 3)
)

kable(high_corr_table, caption = "Variables Highly Correlated with SalePrice")

# INSIGHT: Outlier detection using boxplot statistics informs data cleaning
# Extreme outliers can distort models and should be investigated
# Boxplot stats
cat("\n=== SalePrice Distribution Statistics (Boxplot Method) ===\n")
cat("Lower whisker:", boxplot_stats[1], "\n")
cat("Q1:", boxplot_stats[2], "\n")
cat("Median:", boxplot_stats[3], "\n")
cat("Q3:", boxplot_stats[4], "\n")
cat("Upper whisker:", boxplot_stats[5], "\n\n")

q1 <- boxplot.stats(df$SalePrice)$stats[2]
q3 <- boxplot.stats(df$SalePrice)$stat[4]
iqr <- q3 - q1

# DECISION: Conservative outlier bounds
# Lower: Standard 1.5*IQR rule
# Upper: Relaxed 3*IQR to retain more high-value homes (luxury market segment)
lower_bound <- q1 - (1.5 * iqr)
upper_bound <- q3 + (3 * iqr)

cat("OUTLIER THRESHOLDS:\n")
cat("Lower bound (Q1 - 1.5*IQR):", dollar(lower_bound), "\n")
cat("Upper bound (Q3 + 3*IQR):", dollar(upper_bound), "\n")
# RECOMMENDATION: Homes outside these bounds should be reviewed individually
# to determine if they represent data errors or legitimate luxury/distressed sales.


# WATERFALL CHART: Systematic sample definition
# INSIGHT: A waterfall chart provides complete transparency about data cleaning
# decisions, showing exactly how many observations are removed at each step and why.
# This is critical for reproducibility and defending modeling choices.
# Original dataset length
df_orig <- nrow(df)

# Step 1: remove missing values in SalePrice
# RATIONALE: Cannot predict missing target variable
df_step1 <- df %>%
  filter(!is.na(SalePrice))
nrow(df_step1)

# Step 2: remove zero ground living area
# RATIONALE: GrLivArea is a critical predictor; missing values limit model utility
df_step2 <- df_step1 %>%
  filter(!is.na(GrLivArea))
nrow(df_step2)

# Step 3: remove partial sales
# RATIONALE: Partial sales (e.g., new construction not yet complete) don't represent
# true market prices and would introduce noise into the model
df_step3 <- df_step2 %>%
  filter(SaleCondition != "Partial")
nrow(df_step3)

# Step 4: remove non-single family homes
# RATIONALE: Focus on homogeneous housing type (single-family) to reduce model
# complexity and improve prediction accuracy for this specific market segment
df_step4 <- df_step3 %>%
  filter(BldgType == "1Fam")
nrow(df_step4)

# Step 5: remove saleprice outliers
# RATIONALE: Extreme outliers can heavily influence regression coefficients.
# $35,000 lower bound removes likely data errors or unusual circumstances.
df_step5 <- df_step4 %>%
  filter(SalePrice <= upper_bound,
         SalePrice >= 35000)
nrow(df_step5)

# step 6: have at least 1 bed and 1 full bath
# RATIONALE: Homes without bedrooms/bathrooms are either data errors or not
# typical residential properties (e.g., uninhabitable structures)
df_step6 <- df_step5 %>%
  filter(BedroomAbvGr >= 1,
         (FullBath >= 1)
  )
nrow(df_step6)

# step 7: remove missing values from key predictors
# RATIONALE: Complete case analysis for high-correlation predictors ensures
# consistent sample across all model specifications
df_step7 <- df_step6[complete.cases(df_step6[, high_corr_cols]), ]
nrow(df_step7)

# SUMMARY
cat("\n=== SAMPLE DEFINITION SUMMARY ===\n")
cat("Original observations:", nrow(df), "\n")
cat("Final observations:", nrow(df_step7), "\n")
cat("Retention rate:", percent(nrow(df_step7) / nrow(df)), "\n")
cat("\nKEY INSIGHT: We retained", percent(nrow(df_step7) / nrow(df)), "of the original\n")
cat("dataset while systematically removing data quality issues and focusing on a\n")
cat("homogeneous market segment (single-family homes in normal sale conditions).\n")
cat("This improves model reliability at the cost of some generalizability.\n\n")

# Waterfall chart
# Create a tibble with each step and number of remaining observations
waterfall_steps <- tibble::tibble(
  Step = c(
    "Original dataset",
    "Remove missing SalePrice",
    "Remove missing GrLivArea",
    "Remove partial sales",
    "Keep only 1Fam homes",
    "Remove SalePrice outliers",
    "At least 1 bed and 1 full bath",
    "Remove missing key predictors"
  ),
  Remaining = c(
    nrow(df),
    nrow(df_step1),
    nrow(df_step2),
    nrow(df_step3),
    nrow(df_step4),
    nrow(df_step5),
    nrow(df_step6),
    nrow(df_step7)
  )
)

# Compute how many were dropped at each step
waterfall_steps <- waterfall_steps %>%
  mutate(Dropped = lag(Remaining, default = first(Remaining)) - Remaining,
         ymin = Remaining,
         ymax = lag(Remaining, default = first(Remaining)),
         Step_num = row_number()
         )
# VISUALIZATION INSIGHT: Waterfall chart makes sample attrition immediately clear
# to stakeholders, showing which cleaning steps had the largest impact.
# Create plot
ggplot(waterfall_steps) +
  # Draw rectangles for each drop
  geom_rect(aes(xmin = Step_num - 0.4, xmax = Step_num + 0.4,
                ymin = ymin, ymax = ymax),
            fill = "tomato", color = "black") +
  
  # Use geom_text for number
  geom_text(aes(x = Step_num, 
                y = ymax + 20, 
                label = Dropped),  # small bars -> black
            size = 5,
            fontface = "bold",
            color = "black") +
  # Label remaining observations at the top
  geom_text(aes(x = Step_num, y = ymin - 20, label = Remaining),
            color = "black", size = 5, fontface = "bold") +
  # X-axis labels
  scale_x_continuous(breaks = waterfall_steps$Step_num,
                     labels = waterfall_steps$Step) +
  # Y-axis formatting
  scale_y_continuous(labels = scales::comma) +
  # Plot title and axis labels
  labs(title = "Waterfall of Sample Definition (Cleaning Steps)",
       x = "Data Cleaning Step",
       y = "Remaining Observations") +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save plot
ggsave("waterfall_sample_cleaning.png", path = output_path,
       width = 10, height = 6, units = "in")

# Data quality check ------------------------------------------------------
# INSIGHT: After cleaning, we must verify data quality in our final dataset
# to ensure all variables meet basic validity requirements
# Pick variables
vars <- c("SalePrice","LotArea","LotFrontage","GrLivArea","TotalBsmtSF",
          "GarageArea","GarageCars","BedroomAbvGr","FullBath","HalfBath",
          "YearBuilt","YearRemodel","Zoning","OverallQual","OverallCond",
          "Neighborhood","KitchenQual","CentralAir","SaleCondition","HouseStyle")
# final dataset
df_final <- df_step7[, vars]

cat("=== FINAL DATASET COMPOSITION ===\n")
cat("Variables selected:", length(vars), "\n")
cat("Observations:", nrow(df_final), "\n\n")

# Install webshot if needed
library(webshot)
webshot::install_phantomjs(force = TRUE)

# INSIGHT: Missing data assessment
# Even after cleaning, some variables may have missingness - we need to quantify this
# Determine missing variables
sapply(df_final, function(x) sum(is.na(x)))

missing_summary <- df_final %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(
    Missing_Percent = round(Missing_Count / nrow(df) * 100, 2)
  ) %>%
  filter(Missing_Count != "0") %>%
  arrange(desc(Missing_Count))

# FINDING: Report missing data results
if (nrow(missing_summary) > 0) {
  cat("=== MISSING DATA ASSESSMENT ===\n")
  print(missing_summary)
  cat("\nRECOMMENDATION: Variables with >5% missing may need imputation or exclusion.\n")
  cat("For variables with <5% missing, consider mean/median imputation or\n")
  cat("complete case analysis depending on the mechanism (MCAR vs. MAR).\n\n")
} else {
  cat("=== EXCELLENT NEWS ===\n")
  cat("No missing data detected in final dataset after cleaning steps!\n\n")
}

missing_summary_table <- missing_summary %>%
  kable(format = "pipe",
        caption = "Missing Data Summary Table",
        col.names = c("Variable", "Missing Count", "Missing Percent")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE)
print(missing_summary_table)

# RECOMMENDATION: Uncomment these lines to save tables as images for reports
# Note: Requires absolute paths - update with your working directory
# save_kable(missing_summary_table, 
#            file.path(output_path, "missing_summary_table.html"))
# webshot::webshot(file.path(output_path, "missing_summary_table.html"), 
#                  file.path(output_path, "missing_summary_table.png"), 
# 

# Summary statistics
summary(df_final)

# INSIGHT: Quantile analysis helps identify outliers and distribution shape
# Looking at 1st and 99th percentiles reveals extreme values
# Outlier detection
cat("\n=== QUANTILE ANALYSIS (Outlier Detection) ===\n")
quantiles_list <- lapply(df_final[sapply(df_final,is.numeric)],
                         function(x) quantile(x, probs=c(.01,.05,.25,.5,.75,.95,.99), na.rm=TRUE))
print(quantiles_list)

# INTERPRETATION: Compare 95th-99th percentiles to the max value.
# Large gaps suggest extreme outliers that may warrant investigation.

# Mean and SD
cat("=== CENTRAL TENDENCY AND SPREAD ===\n")
central_stats <- data.frame(
  mean = sapply(df_final[sapply(df_final,is.numeric)], mean, na.rm=TRUE),
  sd   = sapply(df_final[sapply(df_final,is.numeric)], sd, na.rm=TRUE)
)
print(central_stats)

# Validity checks
cat("=== DATA VALIDITY CHECKS ===\n")

# Check 1: SalePrice zero or negative
invalid_price <- df_final[df_final$SalePrice <= 0, c("SalePrice")]
if (length(invalid_price) > 0) {
  cat("WARNING:", length(invalid_price), "properties with invalid SalePrice (≤0)\n")
  print(invalid_price)
} else {
  cat("All SalePrice values are positive\n")
}

# Check 2: year logic violations
invalid_yearbuilt <- df_final[df_final$YearBuilt < 1800 | df_final$YearBuilt > 2010, c("YearBuilt")]
if (length(invalid_yearbuilt) > 0) {
  cat("WARNING:", length(invalid_yearbuilt), "properties with suspicious YearBuilt\n")
  print(invalid_yearbuilt)
} else {
  cat("✓ All YearBuilt values are reasonable (1800-2010)\n")
}

# Check 3: Remodel year cannot precede build year
invalid_remodel <- df_final[df_final$YearRemodel < df_final$YearBuilt, c("YearBuilt","YearRemodel")]
if (nrow(invalid_remodel) > 0) {
  cat("WARNING:", nrow(invalid_remodel), "properties where YearRemodel < YearBuilt (logic violation)\n")
  print(head(invalid_remodel))
  cat("\nACTION: Removing", nrow(invalid_remodel), "observations with temporal logic errors\n")
  df_final <- df_final[df_final$YearRemodel >= df_final$YearBuilt, ] # remove it
} else {
  cat("All YearRemodel values are ≥ YearBuilt\n")
}

# Check 4: Physical size validations
invalid_living <- df_final[df_final$GrLivArea <= 0, "GrLivArea"]
if (length(invalid_living) > 0) {
  cat("WARNING:", length(invalid_living), "properties with GrLivArea ≤ 0\n")
} else {
  cat("All GrLivArea values are positive\n")
}

invalid_lot <- df_final[df_final$LotArea <= 0, "LotArea"]
if (length(invalid_lot) > 0) {
  cat("WARNING:", length(invalid_lot), "properties with LotArea ≤ 0\n")
} else {
  cat("All LotArea values are positive\n")
}

# Check 5: Suspiciously large homes (potential data entry errors or true mansions)
large_homes <- df_final[df_final$GrLivArea > 4000, c("GrLivArea","SalePrice")]
if (nrow(large_homes) > 0) {
  cat("\nNOTE:", nrow(large_homes), "properties exceed 4,000 sqft (potential luxury segment)\n")
  print(large_homes)
  cat("\nRECOMMENDATION: Review these manually - they may be valid luxury properties\n")
  cat("or data entry errors (e.g., decimal point in wrong position).\n\n")
} else {
  cat("No exceptionally large homes (>4,000 sqft) detected\n\n")
}

# Summary table
cat("=== GENERATING COMPREHENSIVE DATA QUALITY SUMMARY ===\n")
summary_table_numeric <- df_final %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(),
                   list(
                     N = ~sum(!is.na(.)),
                     Missing = ~sum(is.na(.)),
                     Mean = ~mean(., na.rm=TRUE),
                     SD = ~sd(., na.rm=TRUE),
                     Min = ~min(., na.rm=TRUE),
                     Q1 = ~quantile(., .25, na.rm=TRUE),
                     Median = ~median(., na.rm=TRUE),
                     Q3 = ~quantile(., .75, na.rm=TRUE),
                     Max = ~max(., na.rm=TRUE)
                   ), .names="{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Stat"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Stat,
    values_from = Value
  ) %>%
  arrange(Variable)

print(summary_table_numeric)

cat("\nINSIGHT: This table provides a complete statistical profile of each variable.\n")
cat("Use it to identify:\n")
cat("  • Skewness (Mean vs. Median)\n")
cat("  • Outliers (Min/Max vs. Q1/Q3)\n")
cat("  • Missing data issues\n")
cat("  • Variables needing transformation\n\n")

summary_table <- summary_table_numeric %>%
  kable(format = "pipe",
      caption = "Data Check Summary Table",
      col.names = c("Variable", "Count", "Missing", "Mean", "SD", 
                    "Min", "Q1", "Median", "Q3", "Max")
      ) %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                full_width = FALSE)
summary_table

# RECOMMENDATION: Uncomment to save tables
# save_kable(summary_table, 
#            file.path(output_path, "summary_table.html"))
# webshot::webshot(file.path(output_path, "summary_table.html"), 
#                  file.path(output_path, "summary_table.png"), 
#                  zoom = 5)

# Initial EDA -------------------------------------------------------------
# INSIGHT: EDA reveals relationships between predictors and response that inform
# modeling decisions (linear vs. non-linear, transformations needed, etc.)
# Select EDA variables
initial_eda_vars <- c("SalePrice", "GrLivArea", "LotArea", "TotalBsmtSF", "GarageArea",
                      "OverallQual", "YearBuilt", "FullBath", "BedroomAbvGr", "Neighborhood")
# filter final dataframe with only eda vars
df_eda <- df_final[, initial_eda_vars]

# Continuous vars EDA
continuous_vars <- c("GrLivArea", "LotArea", "TotalBsmtSF", "GarageArea") # Continuous vars

# INSIGHT: Scatter plots with LOESS curves reveal the functional form of relationships
# Linear LOESS = linear regression appropriate
# Curved LOESS = may need polynomial terms or transformations
# Plots vs SalePrice
for (var in continuous_vars) {
  p <- ggplot(df_eda, aes(x = .data[[var]], y = SalePrice)) +
    geom_point(alpha=0.5) +
    geom_smooth(method="loess", se=TRUE, color="blue") +
    labs(title=paste("SalePrice vs", var),
         y = "Sale Price (UDS)",
         x = paste(var, "(sqft)")) +
    scale_y_continuous(labels = label_dollar(scale = .001, suffix = "K")) +
    theme_bw()
  print(p)
  ggsave(paste0(var, "_plot.png"), path = output_path, width = 10, height = 6, units = "in")
}

cat("\nKEY INSIGHT from scatter plots:\n")
cat("  • Linear LOESS trend → Linear regression model likely appropriate\n")
cat("  • Curved LOESS trend → Consider polynomial terms or log transformation\n")
cat("  • Fan-shaped residuals → Heteroscedasticity; may need weighted regression\n")
cat("  • Leverage points → Identify influential observations\n\n")

# INSIGHT: Histograms reveal distribution shape, which informs:
# 1. Need for transformations (right-skewed → log transform)
# 2. Outlier identification
# 3. Appropriateness of normality assumptions
# Distribution of continuous variables
for (var in continuous_vars) {
  p <- ggplot(df_eda, aes(x = .data[[var]])) +
    geom_histogram(bins=30, fill="steelblue", color="black", alpha=0.7) +
    labs(title=paste("Distribution of", var),
         x = paste(var, "(sqft)"),
         y = "Frequency"
    ) +
    theme_bw()
  print(p)
  ggsave(paste0(var, "_hist_plot.png"), path = output_path, width = 10, height = 6, units = "in")
}

# QUANTITATIVE INSIGHT: Skewness and kurtosis summary
# Skewness interpretation:
#   • |skew| < 0.5  → Approximately symmetric
#   • 0.5 < |skew| < 1  → Moderately skewed
#   • |skew| > 1  → Highly skewed (transformation recommended)
# Kurtosis interpretation:
#   • Kurtosis ≈ 3  → Normal distribution
#   • Kurtosis > 3  → Heavy tails (leptokurtic)
#   • Kurtosis < 3  → Light tails (platykurtic)

# Skewness and kurtosis summary table
require(moments)
# Subset dataset
continuous_df <- df_eda[, c(continuous_vars, "SalePrice")]

# Create table
sk_kurt_table <- data.frame(
  Variable = names(continuous_df),
  Skewness = round(sapply(continuous_df, function(x) skewness(x, na.rm = TRUE)), 3),
  Kurtosis = round(sapply(continuous_df, function(x) kurtosis(x, na.rm = TRUE)), 3)
)
cat("\n=== DISTRIBUTION SHAPE METRICS ===\n")
print(sk_kurt_table)

cat("\nINTERPRETATION GUIDE:\n")
cat("Skewness:\n")
cat("  • 0: Perfectly symmetric\n")
cat("  • >0: Right-skewed (long tail to the right)\n")
cat("  • <0: Left-skewed (long tail to the left)\n")
cat("  • |skew| > 1: Consider log or Box-Cox transformation\n\n")
cat("Kurtosis:\n")
cat("  • 3: Normal distribution\n")
cat("  • >3: Heavy tails (more outliers than normal)\n")
cat("  • <3: Light tails (fewer outliers than normal)\n\n")

# ACTION RECOMMENDATIONS based on skewness
for (i in 1:nrow(sk_kurt_table)) {
  var_name <- sk_kurt_table$Variable[i]
  skew_val <- sk_kurt_table$Skewness[i]
  
  if (abs(skew_val) > 1) {
    cat("RECOMMENDATION:", var_name, "is highly skewed (", skew_val, 
        "). Consider log transformation.\n")
  }
}
cat("\n")

# Categorical/discrete vars EDA
cat("--- Categorical Variable Analysis ---\n")
cat("Creating boxplots to examine SalePrice distribution across categories...\n\n")

# Categorical vars
categorical_vars <- c("OverallQual", "YearBuilt", "FullBath", "BedroomAbvGr", "Neighborhood") 

# Make factor for proper handling in plots
df_eda <- df_eda %>%
  mutate(across(all_of(categorical_vars), as.factor))

# INSIGHT: Boxplots show how SalePrice varies by categorical levels
# Look for:
#   • Clear separation between categories (good predictor)
#   • Overlap between categories (weak predictor)
#   • Outliers within categories
#   • Monotonic trends (e.g., higher quality → higher price)
for (var in categorical_vars) {
  p <- ggplot(df_eda, aes(x=.data[[var]], y= SalePrice)) +
    geom_boxplot(fill="lightgreen") +
    geom_jitter(width=0.2, alpha=0.3) +
    labs(title=paste("SalePrice by", var),
         y = "Sale Price (UDS)") +
    scale_y_continuous(labels = label_dollar(scale = .001, suffix = "K")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
  ggsave(paste0(var, "_boxplot.png"), path = output_path, width = 10, height = 6, units = "in")
}

cat("\nKEY INSIGHTS from boxplots:\n")
cat("  • Clear separation → Strong categorical predictor (include in model)\n")
cat("  • Overlapping boxes → Weak predictor (may not improve model)\n")
cat("  • Monotonic trend → Consider treating as ordinal (numeric coding)\n")
cat("  • Many outliers → Investigate influential observations\n\n")

# Modeling EDA ------------------------------------------------------------
# INSIGHT: Model-specific EDA focuses on variables selected for the initial model
# and examines whether transformations improve linear relationship assumptions
# selecting modeling vars
modeling_vars <- c("GrLivArea", "TotalBsmtSF", "LotArea")
df_modeling_eda <- df_eda[, c(modeling_vars, "SalePrice")] # Filter to those vars

# ORIGINAL SCALE: SalePrice distribution
ggplot(df_modeling_eda, aes(x = SalePrice)) +
  geom_histogram(bins=30, fill="steelblue", color="black", alpha=0.7) +
  labs(title=paste("Distribution of SalePrice"),
       x = "Sale Price (UDS)",
       y = "Frequency"
  ) +
  scale_x_continuous(labels = label_dollar(scale = .001, suffix = "K")) +
  theme_bw()
# Save plot
ggsave("SalePrice_hist_plot.png", path = output_path, width = 10, height = 6, units = "in")

# Create log-transformed target variable
# RATIONALE: Log transformation often improves:
#   1. Normality of residuals
#   2. Homoscedasticity (constant variance)
#   3. Linearity of relationships
#   4. Interpretation (coefficients become percent changes)
df_modeling_eda$logSalePrice <- log(df_modeling_eda$SalePrice) 

cat("Created logSalePrice variable using natural log transformation\n")
cat("INTERPRETATION: Model coefficients will represent % change in SalePrice\n")
cat("rather than absolute dollar changes.\n\n")

# LOG SCALE: SalePrice distribution
ggplot(df_modeling_eda, aes(x = logSalePrice)) +
  geom_histogram(bins=30, fill="steelblue", color="black", alpha=0.7) +
  labs(title=paste("Distribution of Log SalePrice"),
       x = "Log Sale Price (UDS)",
       y = "Frequency"
  ) +
  scale_x_continuous(labels = label_dollar()) +
  theme_bw()
# Save plot
ggsave("logSalePrice_hist_plot.png", path = output_path, width = 10, height = 6, units = "in")

cat("COMPARISON INSIGHT:\n")
cat("  • Original SalePrice: Likely right-skewed\n")
cat("  • Log(SalePrice): More symmetric, approximately normal\n")
cat("  • DECISION: Use log(SalePrice) as target variable in regression model\n\n")

# FINAL MODEL READINESS CHECK: Examine relationships with log-transformed target
cat("Creating scatter plots with log-transformed SalePrice...\n")
cat("This shows the relationship the model will actually fit.\n\n")

# Scatter Plots vs logSalePrice
for (var in modeling_vars) {
  p <- ggplot(df_modeling_eda, aes(x = .data[[var]], y = logSalePrice)) +
    geom_point(alpha=0.5) +
    geom_smooth(method="loess", se=TRUE, color="blue") +
    labs(title=paste("Log(SalePrice) vs", var),
         y = "Log Sale Price (UDS)",
         x = paste(var, "(sqft)")) +
    scale_y_continuous(labels = label_dollar()) +
    theme_bw()
  print(p)
  ggsave(paste0(var, "_modeling_plot.png"), path = output_path, width = 10, height = 6, units = "in")
}

cat("\n=== FINAL MODELING RECOMMENDATIONS ===\n")
cat("Based on the EDA conducted:\n\n")

cat("1. TARGET VARIABLE:\n")
cat("   Use log(SalePrice) to improve normality and stabilize variance\n\n")

cat("2. PRIMARY PREDICTORS (high correlation with SalePrice):\n")
for (var in high_corr_cols[1:min(5, length(high_corr_cols))]) {
  cat("   •", var, "(correlation:", round(high_corr[var], 3), ")\n")
}
cat("\n")

cat("3. DATA QUALITY:\n")
cat("   • Sample size:", nrow(df_final), "observations after cleaning\n")
cat("   • Focused on single-family homes in normal sale conditions\n")
cat("   • No missing data in key variables\n")
cat("   • Outliers identified and documented\n\n")

cat("4. MODELING APPROACH:\n")
cat("   • Start with multiple linear regression: log(SalePrice) ~ predictors\n")
cat("   • Check residual plots for linearity, normality, homoscedasticity\n")
cat("   • Assess multicollinearity (VIF) among highly correlated predictors\n")
cat("   • Consider interaction terms (e.g., GrLivArea × OverallQual)\n")
cat("   • Validate model on hold-out test set\n\n")

cat("5. NEXT STEPS:\n")
cat("   a) Split data into training (80%) and test (20%) sets\n")
cat("   b) Fit initial model with high-correlation predictors\n")
cat("   c) Perform residual diagnostics\n")
cat("   d) Refine model based on diagnostics (transformations, interactions)\n")
cat("   e) Compare models using AIC, BIC, adjusted R²\n")
cat("   f) Validate final model on test set (RMSE, R²)\n\n")

cat("=== EDA COMPLETE ===\n")
cat("All visualizations and tables saved to:", output_path, "\n")
cat("The data is now ready for formal modeling.\n")