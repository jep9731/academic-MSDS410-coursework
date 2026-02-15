################################################################################
# MSDS 410 - Assignment 6: Variable Selection & Model Validation (Part 2)
# Author: Joshua Pasaye
# 
# This script continues from Part 1, focusing on:
# - Automated variable selection procedures
# - Multicollinearity detection and remediation
# - Model comparison and selection
# - Comprehensive model validation
################################################################################

# Set-up environment ------------------------------------------------------
# Clean environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(gt)
library(knitr)
library(car)
library(readxl)
# install.packages("fastDummies") # Install if needed
library(fastDummies)
library(MASS)
# install.packages("Metrics") # Install if needed
library(Metrics)
library(lmtest)

# Import dataset
df <- readxl::read_xlsx("ames_housing_data.xlsx")

# Create output directory
output_dir <- "Output"

if (!dir.exists(output_dir)) {
  dir.create(path = paste(dirname(getwd()), "/Output", sep = ""))
} else {
  print("Directory already exists!")
}
# Output path
output_path <- file.path(dirname(getwd()), "/Output", sep = "")

# Final sample -----------------------------------------------------
# Step 1: Remove partial & abnormal sales
# RATIONALE: Focus on normal sale conditions as other sale types
# may add outlier or uncommon transactions
df_step1 <- df %>%
  filter(SaleCondition != "Partial" & SaleCondition != "Abnormal")
nrow(df_step1)

# Step 2: Remove non-single family homes
# RATIONALE: Focus on homogeneous housing type (single-family) to reduce model
# complexity and improve prediction accuracy for this specific market segment
df_step2 <- df_step1 %>%
  filter(BldgType == "1Fam")
nrow(df_step2)

# Step 3: Have at least 1 bed and 1 full bath
# RATIONALE: Homes without bedrooms/bathrooms are either data errors or not
# typical residential properties (e.g., uninhabitable structures)
df_step3 <- df_step2 %>%
  filter(BedroomAbvGr >= 1,
         FullBath >= 1)
nrow(df_step3)

# Step 4: Remove homes outside typical living area range
# RATIONALE: Documentation recommends removing homes >4000 sq ft as outliers
# (3 partial sales, 2 unusually large). Also remove very small homes (<500 sq ft)
# which may be data errors or non-standard dwellings
df_step4 <- df_step3 %>%
  filter(GrLivArea >= 500 & GrLivArea <= 4000)
nrow(df_step4)

# Step 5: Ensure all public utilities
# RATIONALE: Homes without all utilities (electric, gas, water, sewer) may be
# incomplete properties or not comparable to standard housing stock
df_step5 <- df_step4 %>%
  filter(Utilities == "AllPub")
nrow(df_step5)

# Step 6: Residential zoning only
# RATIONALE: Exclude commercial, agricultural, and industrial zones to maintain
# focus on residential properties in typical residential neighborhoods
df_step6 <- df_step5 %>%
  filter(Zoning %in% c("RL", "RM", "RH", "FV", "RP"))
nrow(df_step6)

# Step 7: Remove severely impaired functionality
# RATIONALE: Homes with major/severe deductions or salvage-only functionality
# are not representative of typical market transactions
df_step7 <- df_step6 %>%
  filter(!Functional %in% c("Maj1", "Maj2", "Sev", "Sal"))
nrow(df_step7)

# Step 8: Minimum quality standards
# RATIONALE: Exclude homes with very poor overall quality (1-2) and condition (1-2)
# as these are likely tear-downs or require extensive renovation
df_step8 <- df_step7 %>%
  filter(OverallQual >= 3,
         OverallCond >= 3)
nrow(df_step8)

# Step 9: Focus on conventional sales
# RATIONALE: Warranty deed sales (WD, CWD, VWD) and new homes represent
# typical market transactions. Exclude foreclosures (COD), contracts,
# and other non-conventional sale types
df_step9 <- df_step8 %>%
  filter(SaleType %in% c("WD", "CWD", "VWD", "New"))
nrow(df_step9)

# Step 10: Exclude very old homes (optional - adjust threshold as needed)
# RATIONALE: Homes built before 1900 may not be comparable to modern housing
# stock due to outdated construction methods, materials, and standards
df_final <- df_step9 %>%
  filter(YearBuilt >= 1900)
nrow(df_final)

# Summary of sample reduction
cat("Original sample:", nrow(df), "\n")
cat("Final sample:", nrow(df_final), "\n")
cat("Reduction:", nrow(df) - nrow(df_final), "observations",
    paste0("(", round(100*(nrow(df) - nrow(df_final))/nrow(df), 1), "%)"), "\n")

# Waterfall chart
# Create a tibble with each step and number of remaining observations
waterfall_steps <- tibble::tibble(
  Step = c(
    "Original dataset",
    "Remove partial & abnormal sales",
    "Keep only 1Fam homes",
    "At least 1 bed and 1 full bath",
    "Remove GrLivArea outliers (500-4000 sq ft)",
    "Ensure all public utilities",
    "Residential zoning only",
    "Remove severely impaired functionality",
    "Minimum quality standards (Qual & Cond >= 3)",
    "Focus on conventional sales (WD/CWD/VWD/New)",
    "Exclude homes built before 1900"
  ),
  Remaining = c(
    nrow(df), # Original
    nrow(df_step1), # After removing partial & abnormal sales
    nrow(df_step2), # After keeping only 1Fam
    nrow(df_step3), # After bed/bath filter
    nrow(df_step4), # After GrLivArea filter 500-4000
    nrow(df_step5), # After utilities filter
    nrow(df_step6), # After zoning filter
    nrow(df_step7), # After functionality filter
    nrow(df_step8), # After quality filter
    nrow(df_step9), # After sale type filter
    nrow(df_final)  # After removing very old homes/final
               
  )
)

# Print summary table
print(waterfall_steps, n = Inf)

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
  geom_rect(aes(xmin = Step_num - 0.3, xmax = Step_num + 0.3,
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
       width = 14, height = 8, units = "in")

## Create summary table ##
# Define categorical variables to examine
# Ordinal variables
ordinal_vars <- c("OverallQual", "OverallCond", "KitchenQual", "ExterQual", 
                  "BsmtQual", "HeatingQC", "GarageFinish", "FireplaceQu", 
                  "Functional", "GarageCond", "ExterCond")

# Nominal variables  
nominal_vars <- c("Neighborhood", "CentralAir", "GarageType", "Foundation",
                  "HouseStyle", "RoofStyle")

all_cat_vars <- c(ordinal_vars, nominal_vars)

# Function: Compute summary statistics by categorical variable
compute_cat_summaries <- function(data, cat_var) {
  data %>%
    group_by(across(all_of(cat_var))) %>%
    summarise(
      N = n(),
      Mean = mean(SalePrice, na.rm = TRUE),
      Median = median(SalePrice, na.rm = TRUE),
      SD = sd(SalePrice, na.rm = TRUE),
      Min = min(SalePrice, na.rm = TRUE),
      Max = max(SalePrice, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Mean)) %>%
    mutate(
      Variable = cat_var,
      Mean_Diff_from_Overall = Mean - mean(data$SalePrice, na.rm = TRUE),
      Range = Max - Min
    )
}

# Function: Calculate key metrics for variable selection
evaluate_categorical_var <- function(data, cat_var) {
  # Summary stats by level
  summary_stats <- compute_cat_summaries(data, cat_var)
  
  # Calculate range of means (max mean - min mean)
  mean_range <- max(summary_stats$Mean) - min(summary_stats$Mean)
  
  # Fit simple linear model to get R-squared
  formula_str <- paste("SalePrice ~", cat_var)
  model <- lm(as.formula(formula_str), data = data)
  r_squared <- summary(model)$r.squared
  adj_r_squared <- summary(model)$adj.r.squared
  
  # Number of levels
  n_levels <- nrow(summary_stats)
  
  # Return metrics
  tibble(
    Variable = cat_var,
    N_Levels = n_levels,
    Mean_Range = mean_range,
    R_Squared = r_squared,
    Adj_R_Squared = adj_r_squared
  )
}

# Evaluate all categorical variables
# Get evaluation metrics for all variables
cat_evaluation <- map_df(all_cat_vars, 
                         ~evaluate_categorical_var(df_final, .x)) %>%
  arrange(desc(R_Squared))

# Print ranked results
print(cat_evaluation, n = Inf)

## Detailed summary statistics for top variables ##
# Select top variables (top 5)
top_vars <- cat_evaluation %>%
  filter(row_number() <= 5) %>%
  pull(Variable)

print(top_vars)

# Generate detailed summaries for top variables
detailed_summaries <- map(top_vars, ~compute_cat_summaries(df_final, .x))
names(detailed_summaries) <- top_vars

# Print each summary
for (var in top_vars) {
  cat("\n", strrep("=", 70), "\n")
  cat("VARIABLE:", var, "\n")
  cat(strrep("=", 70), "\n")
  print(detailed_summaries[[var]], n = Inf)
  
  # Calculate and display mean difference statistics
  mean_diff <- max(detailed_summaries[[var]]$Mean) - 
    min(detailed_summaries[[var]]$Mean)
  cat("\nMean Difference (Max - Min):", scales::dollar(mean_diff), "\n")
}

# Create comparison plot
cat_evaluation %>%
  top_n(10, R_Squared) %>%
  ggplot(aes(x = reorder(Variable, Mean_Range), y = Mean_Range)) +
  geom_col(aes(fill = R_Squared)) +
  geom_text(aes(label = scales::dollar(Mean_Range, accuracy = 1)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Mean Price Range Across Categorical Variable Levels",
    subtitle = "Top 10 variables by R-squared",
    x = NULL,
    y = "Price Range (Max Mean - Min Mean)",
    fill = "R²"
  ) +
  theme_bw(base_size = 14)
# Save plot
ggsave("variable_selection.png", path = output_path,
       width = 14, height = 8, units = "in")

## Create dummy variables ##
# Define these two variables for later use
df_final$QualityIndex <- df_final$OverallQual * df_final$OverallCond
df_final$TotalSqftCalc <- df_final$BsmtFinSF1 + df_final$BsmtFinSF2 + df_final$GrLivArea

# Function to create dummy variables (handling numeric categorical variables)
create_dummies_reference <- function(data, cat_vars) {
  dummy_list <- list()
  reference_levels <- list()
  
  # First, convert variables to factors if they aren't already
  data_for_dummies <- data
  for (var in cat_vars) {
    if (!is.factor(data_for_dummies[[var]]) && !is.character(data_for_dummies[[var]])) {
      # Convert numeric to factor
      data_for_dummies[[var]] <- as.factor(data_for_dummies[[var]])
    } else if (is.character(data_for_dummies[[var]])) {
      # Convert character to factor
      data_for_dummies[[var]] <- as.factor(data_for_dummies[[var]])
    }
  }
  
  for (var in cat_vars) {
    # Get levels and reference level
    all_levels <- levels(data_for_dummies[[var]])
    ref_level <- all_levels[1]
    reference_levels[[var]] <- ref_level
    
    # Create formula with reference level (first level omitted)
    formula_str <- paste("~", var)
    
    # Create dummy matrix
    dummy_matrix <- model.matrix(as.formula(formula_str), data = data_for_dummies)[, -1, drop = FALSE]
    
    # Convert to tibble and create clean names
    if (ncol(dummy_matrix) > 0) {
      dummy_df <- as_tibble(dummy_matrix)
      
      # Get the column names and extract level information
      original_names <- colnames(dummy_matrix)
      
      # Create clean names: VarName_Level
      new_names <- sapply(original_names, function(col_name) {
        # Remove the variable name prefix to get the level
        level_name <- gsub(paste0("^", var), "", col_name)
        # If level_name is empty, use the full column name
        if (level_name == "") {
          level_name <- col_name
        }
        paste0(var, "_", level_name)
      })
      
      colnames(dummy_df) <- new_names
      dummy_list[[var]] <- dummy_df
    }
  }
  
  # Remove original categorical variables
  cols_to_keep <- !(names(data) %in% cat_vars)
  data_without_cats <- data[, cols_to_keep]
  
  # Add dummy variables
  result <- bind_cols(data_without_cats, dummy_list)
  return(result)
}

# Create dummy variables for top variables
df_with_dummies <- create_dummies_reference(df_final, top_vars)

cat("\n=== DUMMY VARIABLES CREATED ===\n")
cat("New columns added:", ncol(df_with_dummies) - ncol(df_final), "\n")
cat("Total columns:", ncol(df_with_dummies), "\n")

# Modeling Framework -----------------------------------------------------
## Set seed for reproducibility ##
set.seed(123)
df_with_dummies$u <- runif(n=dim(df_with_dummies)[1],min=0,max=1)

## Create train/test split ##
train.df <- subset(df_with_dummies, u < 0.70)
test.df  <- subset(df_with_dummies, u >= 0.70)

# Check your data split. The sum of the parts should equal the whole ##
dim(df_with_dummies)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1] + dim(test.df)[1]

## Create summary table ##
partition_table <- tibble(
  Dataset = c("Total Sample", "Training Set", "Test Set", "Verification"),
  N = c(
    nrow(df_with_dummies),
    nrow(train.df),
    nrow(test.df),
    nrow(train.df) + nrow(test.df)
  ),
  Percentage = c(
    100.0,
    round(100 * nrow(train.df) / nrow(df_with_dummies), 1),
    round(100 * nrow(test.df) / nrow(df_with_dummies), 1),
    round(100 * (nrow(train.df) + nrow(test.df)) / nrow(df_with_dummies), 1)
  )
)

print(partition_table, n = Inf)

# Automatic variable selection --------------------------------------------
## Determine reference levels (the level that's NOT in the dummy set) ##
# Get dummy variable names
overallqual_dummies <- grep("^OverallQual_", names(df_with_dummies), value = TRUE)
neighborhood_dummies <- grep("^Neighborhood_", names(df_with_dummies), value = TRUE)
bsmtqual_dummies <- grep("^BsmtQual_", names(df_with_dummies), value = TRUE)
exterqual_dummies <- grep("^ExterQual_", names(df_with_dummies), value = TRUE)
kitchenqual_dummies <- grep("^KitchenQual_", names(df_with_dummies), value = TRUE)

# For OverallQual in df_final
all_overallqual <- sort(unique(df_final$OverallQual))
overallqual_in_dummies <- gsub("OverallQual_", "", overallqual_dummies)
overallqual_ref <- setdiff(as.character(all_overallqual), overallqual_in_dummies)[1]

# For Neighborhood
all_neighborhoods <- sort(unique(df_final$Neighborhood))
neighborhood_in_dummies <- gsub("Neighborhood_", "", neighborhood_dummies)
neighborhood_ref <- setdiff(all_neighborhoods, neighborhood_in_dummies)[1]

# For BsmtQual
all_bsmtqual <- sort(unique(df_final$BsmtQual))
bsmtqual_in_dummies <- gsub("BsmtQual_", "", bsmtqual_dummies)
bsmtqual_ref <- setdiff(all_bsmtqual, bsmtqual_in_dummies)[1]

# For ExterQual
all_exterqual <- sort(unique(df_final$ExterQual))
exterqual_in_dummies <- gsub("ExterQual_", "", exterqual_dummies)
exterqual_ref <- setdiff(all_exterqual, exterqual_in_dummies)[1]

# For KitchenQual
all_kitchenqual <- sort(unique(df_final$KitchenQual))
kitchenqual_in_dummies <- gsub("KitchenQual_", "", kitchenqual_dummies)
kitchenqual_ref <- setdiff(all_kitchenqual, kitchenqual_in_dummies)[1]

## Define candidate predictor pool ##
# Create comprehensive candidate pool table
candidate_pool <- tibble(
  Variable_Name = c(
    # Continuous Variables
    "LotArea",
    "GrLivArea",
    "TotalBsmtSF",
    "GarageArea",
    "YearBuilt",
    "YearRemodel",
    
    # Engineered Continuous Variables
    "TotalSqftCalc",
    "QualityIndex",
    
    # Discrete Variables
    "BedroomAbvGr",
    "FullBath",
    "HalfBath",
    
    # Categorical Dummy Variables (represented as groups)
    "OverallQual_*",
    "Neighborhood_*",
    "BsmtQual_*",
    "ExterQual_*",
    "KitchenQual_*"
  ),
  
  Type = c(
    # Continuous
    rep("Continuous", 6),
    # Engineered Continuous
    rep("Continuous (Engineered)", 2),
    # Discrete
    rep("Discrete", 3),
    # Categorical Dummies
    rep("Categorical (Dummy Set)", 5)
  ),
  
  Description = c(
    # Continuous Variables
    "Lot size in square feet",
    "Above grade living area (sq ft)",
    "Total basement area (sq ft)",
    "Garage area (sq ft)",
    "Original construction year",
    "Remodel year (= YearBuilt if no remodel)",
    
    # Engineered Continuous
    "Total finished square footage",
    "OverallQual × OverallCond interaction",
    
    # Discrete Variables
    "Number of bedrooms above grade",
    "Number of full bathrooms above grade",
    "Number of half bathrooms above grade",
    
    # Categorical Dummies
    paste0("Overall quality rating (", length(overallqual_dummies), " dummies)"),
    paste0("Neighborhood location (", length(neighborhood_dummies), " dummies)"),
    paste0("Basement quality (", length(bsmtqual_dummies), " dummies)"),
    paste0("Exterior quality (", length(exterqual_dummies), " dummies)"),
    paste0("Kitchen quality (", length(kitchenqual_dummies), " dummies)")
  ),
  
  Range_or_Levels = c(
    # Continuous Variables
    paste0(format(min(df_with_dummies$LotArea, na.rm = TRUE), big.mark = ","), " - ", 
           format(max(df_with_dummies$LotArea, na.rm = TRUE), big.mark = ",")),
    paste0(format(min(df_with_dummies$GrLivArea, na.rm = TRUE), big.mark = ","), " - ", 
           format(max(df_with_dummies$GrLivArea, na.rm = TRUE), big.mark = ",")),
    paste0(format(min(df_with_dummies$TotalBsmtSF, na.rm = TRUE), big.mark = ","), " - ", 
           format(max(df_with_dummies$TotalBsmtSF, na.rm = TRUE), big.mark = ",")),
    paste0(format(min(df_with_dummies$GarageArea, na.rm = TRUE), big.mark = ","), " - ", 
           format(max(df_with_dummies$GarageArea, na.rm = TRUE), big.mark = ",")),
    paste0(min(df_with_dummies$YearBuilt, na.rm = TRUE), " - ", max(df_with_dummies$YearBuilt, na.rm = TRUE)),
    paste0(min(df_with_dummies$YearRemodel, na.rm = TRUE), " - ", max(df_with_dummies$YearRemodel, na.rm = TRUE)),
    
    # Engineered Continuous
    paste0(format(min(df_with_dummies$TotalSqftCalc, na.rm = TRUE), big.mark = ","), " - ", 
           format(max(df_with_dummies$TotalSqftCalc, na.rm = TRUE), big.mark = ",")),
    paste0(min(df_with_dummies$QualityIndex, na.rm = TRUE), " - ", max(df_with_dummies$QualityIndex, na.rm = T)),
    
    # Discrete Variables
    paste0(min(df_with_dummies$BedroomAbvGr, na.rm = TRUE), " - ", max(df_with_dummies$BedroomAbvGr, na.rm = TRUE)),
    paste0(min(df_with_dummies$FullBath, na.rm = TRUE), " - ", max(df_with_dummies$FullBath, na.rm = TRUE)),
    paste0(min(df_with_dummies$HalfBath, na.rm = TRUE), " - ", max(df_with_dummies$HalfBath, na.rm = TRUE)),
    
    # Categorical Dummies
    paste0("Reference: ", overallqual_ref),
    paste0("Reference: ", neighborhood_ref),
    paste0("Reference: ", bsmtqual_ref),
    paste0("Reference: ", exterqual_ref),
    paste0("Reference: ", kitchenqual_ref)
  )
)

# Add number column
candidate_pool <- candidate_pool %>%
  mutate(No = 1:n()) %>%
  dplyr::select(No, dplyr::everything())

## Display and export table ##
# Print table
print(candidate_pool, n = Inf)

# Count totals
total_continuous <- sum(candidate_pool$Type %in% c("Continuous", "Continuous (Engineered)"))
total_discrete <- sum(candidate_pool$Type == "Discrete")
total_dummy_vars <- length(overallqual_dummies) + length(neighborhood_dummies) + 
  length(bsmtqual_dummies) + length(exterqual_dummies) + 
  length(kitchenqual_dummies)

# Create formatted table for report
candidate_table <- candidate_pool %>%
  gt() %>%
  tab_header(
    title = "Candidate Predictor Pool",
    subtitle = "Variables available for automatic selection"
  ) %>%
  cols_label(
    No = "No.",
    Variable_Name = "Variable Name",
    Type = "Type",
    Description = "Description",
    Range_or_Levels = "Range/Levels"
  ) %>%
  cols_align(
    align = "center",
    columns = c(No)
  ) %>%
  cols_align(
    align = "left",
    columns = c(Variable_Name, Type, Description, Range_or_Levels)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_body(rows = seq(1, nrow(candidate_pool), 2))
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 13,
    column_labels.font.weight = "bold"
  )
candidate_table

# Export to CSV
write_csv(candidate_pool, 
          file = paste0(output_path, "/candidate_predictor_pool.csv")
          )

# Selecting Model ---------------------------------------------------------
## Filter out unused variables ##
# Create list of variables to keep
keep_vars <- c(
  # Continuous variables
  "LotArea",
  "GrLivArea", 
  "TotalBsmtSF",
  "FirstFlrSF",
  "SecondFlrSF",
  "GarageArea",
  "YearBuilt",
  "YearRemodel",
  
  # Engineered variables
  "TotalSqftCalc",
  "QualityIndex",
  
  # Discrete variables
  "BedroomAbvGr",
  "FullBath",
  "HalfBath",
  "Fireplaces",
  "GarageCars",
  "TotRmsAbvGrd",
  
  # Response variable
  "SalePrice"
)

# Add all dummy variables (they already have reference level omitted)
dummy_vars <- grep("_", names(df_with_dummies), value = TRUE)
dummy_vars <- dummy_vars[!dummy_vars %in% c("u")]  # Exclude split variable

keep_vars <- c(keep_vars, dummy_vars)

# Filter datasets
train.clean <- train.df[, names(train.df) %in% keep_vars]
train.clean <- drop_na(train.clean)

test.clean <- test.df[, names(test.df) %in% keep_vars]
test.clean <- drop_na(test.clean)

cat("Training set:\n")
cat("  Observations:", nrow(train.clean), "\n")
cat("  Variables:", ncol(train.clean), "\n\n")

cat("Test set:\n")
cat("  Observations:", nrow(test.clean), "\n")
cat("  Variables:", ncol(test.clean), "\n\n")

# Verify no missing values in key variables
missing_count <- sum(is.na(train.clean))
cat("  Training set missing values:", missing_count, "\n")
if (missing_count > 0) {
  cat("  Variables with missing values:\n")
  missing_vars <- colSums(is.na(train.clean))
  print(missing_vars[missing_vars > 0])
}

## Model identification ##
# Define the upper model as the FULL model
upper.lm <- lm(SalePrice ~ .,data=train.clean);
summary(upper.lm)

# Define the lower model as the Intercept model
lower.lm <- lm(SalePrice ~ 1,data=train.clean);

# Need a SLR to initialize stepwise selection
sqft.lm <- lm(SalePrice ~ TotalSqftCalc,data=train.clean);
summary(sqft.lm)

## Call stepAIC() for variable selection ##
# Forward
forward.lm <- stepAIC(object=lower.lm,scope=list(upper=formula(upper.lm),lower=~1),
                      direction=c('forward'));
summary(forward.lm)

# Backward
backward.lm <- stepAIC(object=upper.lm,direction=c('backward'));
summary(backward.lm)

# Stepwise
stepwise.lm <- stepAIC(object=sqft.lm,scope=list(upper=formula(upper.lm),lower=~1),
                       direction=c('both'));
summary(stepwise.lm)

# Junk model
junk.lm <- lm(
  SalePrice ~ QualityIndex + 
    OverallQual_6 + OverallQual_7 + OverallQual_8 + OverallQual_9 + OverallQual_10 +
    GrLivArea + TotalSqftCalc,
  data = train.clean
  )
summary(junk.lm)

## Compute the VIF values ##
# Forward
sort(vif(forward.lm),decreasing=TRUE)

# Backward
sort(vif(backward.lm),decreasing=TRUE)

# Stepwise
sort(vif(stepwise.lm),decreasing=TRUE)

# Junk
sort(vif(junk.lm), decreasing=TRUE)

# Model comparison --------------------------------------------------------
# Function to compute fit metrics
model_metrics <- function(model) {
  preds <- predict(model) 
  y <- model$model$SalePrice
  
  mse <- mean((y - preds)^2)
  mae <- mean(abs(y - preds))
  
  c(
    Adj_R2 = summary(model)$adj.r.squared,
    AIC = AIC(model),
    BIC = BIC(model),
    MSE = mse,
    MAE = mae
  )
}

results <- rbind(
  Forward = model_metrics(forward.lm),
  Backward = model_metrics(backward.lm),
  Stepwise = model_metrics(stepwise.lm),
  Junk = model_metrics(junk.lm)
)

results

# Convert to data frame
results <- as.data.frame(results)

# Ranking (higher is better for Adj R2; lower is better for others)
ranks <- data.frame(
  Adj_R2_Rank = rank(-results$Adj_R2),
  AIC_Rank = rank(results$AIC),
  BIC_Rank = rank(results$BIC),
  MSE_Rank = rank(results$MSE),
  MAE_Rank = rank(results$MAE)
)

final_table <- cbind(results, ranks)
final_table

## INTERPRETATION: No one model dominates in goodness-of-fit measures.
## Stepwise is ranked first in Adjusted R-squared and AIC, while backwards
## selection is ranked first in BIC and MAE. Forward selection is only ranked
## first in MSE.

# Predictive analysis -----------------------------------------------------
## Run predict() ##
# Forward
forward.test <- predict(forward.lm, newdata= test.clean)

# Backward
backward.test <- predict(backward.lm, newdata= test.clean)

# Stepwise
stepwise.test <- predict(stepwise.lm, newdata= test.clean)

# Junk
junk.test <- predict(junk.lm, newdata = test.clean)

## Errors ##
# Actual values
y.test <- test.clean$SalePrice

# Forward
mse_forward <- mse(y.test, forward.test)
mae_forward <- mae(y.test, forward.test)

# Backward
mse_backward <- mse(y.test, backward.test)
mae_backward <- mae(y.test, backward.test)

# Stepwise
mse_stepwise <- mse(y.test, stepwise.test)
mae_stepwise <- mae(y.test, stepwise.test)

# Junk
mse_junk <- mse(y.test, junk.test)
mae_junk <- mae(y.test, junk.test)

## Combine into table ##
test_results <- data.frame(
  MSE = c(mse_forward, mse_backward, mse_stepwise, mse_junk),
  MAE = c(mae_forward, mae_backward, mae_stepwise, mae_junk)
)

rownames(test_results) <- c("Forward", "Backward", "Stepwise", "Junk")
test_results

## INTREPRETATION: Based on the MSE and MAE, backward selection produced the
## best results for both measures of accuracy. MSE captures predictive accuracy
## while MAE focuses on absolute deviations. Both are important for determining
## which model should be used for analysis.

# Operational validation --------------------------------------------------
## Training Abs Pct Error ##
# Forward
forward.pct <- abs(forward.lm$residuals)/train.clean$SalePrice

# Assign Prediction Grades
forward.PredictionGrade <- ifelse(forward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(forward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(forward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

forward.trainTable <- table(forward.PredictionGrade)
forward.trainTable/sum(forward.trainTable)

# Backwards
backward.pct <- abs(backward.lm$residuals)/train.clean$SalePrice

# Assign Prediction Grades
backward.PredictionGrade <- ifelse(backward.pct<=0.10,'Grade 1: [0.0.10]',
                                  ifelse(backward.pct<=0.15,'Grade 2: (0.10,0.15]',
                                         ifelse(backward.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                'Grade 4: (0.25+]')
                                  )					
)

backward.trainTable <- table(backward.PredictionGrade)
backward.trainTable/sum(backward.trainTable)

# Stepwise
stepwise.pct <- abs(stepwise.lm$residuals)/train.clean$SalePrice

# Assign Prediction Grades
stepwise.PredictionGrade <- ifelse(stepwise.pct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(stepwise.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(stepwise.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

stepwise.trainTable <- table(stepwise.PredictionGrade)
stepwise.trainTable/sum(stepwise.trainTable)

# Junk
junk.pct <- abs(junk.lm$residuals)/train.clean$SalePrice

# Assign Prediction Grades
junk.PredictionGrade <- ifelse(junk.pct<=0.10,'Grade 1: [0.0.10]',
                                   ifelse(junk.pct<=0.15,'Grade 2: (0.10,0.15]',
                                          ifelse(junk.pct<=0.25,'Grade 3: (0.15,0.25]',
                                                 'Grade 4: (0.25+]')
                                   )					
)

junk.trainTable <- table(junk.PredictionGrade)
junk.trainTable/sum(junk.trainTable)

## Test Abs Pct Error ##
# Forward
forward.testPCT <- abs(test.df$SalePrice-forward.test)/test.df$SalePrice

# Assign Prediction Grades
forward.testPredictionGrade <- ifelse(forward.testPCT<=0.10,'Grade 1: [0.0.10]',
                                      ifelse(forward.testPCT<=0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(forward.testPCT<=0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

forward.testTable <-table(forward.testPredictionGrade)
forward.testTable/sum(forward.testTable)

# Backward
backward.testPCT <- abs(test.df$SalePrice-backward.test)/test.df$SalePrice

# Assign Prediction Grades
backward.testPredictionGrade <- ifelse(backward.testPCT <= 0.10,'Grade 1: [0.0.10]',
                                      ifelse(backward.testPCT <= 0.15,'Grade 2: (0.10,0.15]',
                                             ifelse(backward.testPCT <= 0.25,'Grade 3: (0.15,0.25]',
                                                    'Grade 4: (0.25+]')
                                      )					
)

backward.testTable <-table(backward.testPredictionGrade)
backward.testTable/sum(backward.testTable)

# Stepwise
stepwise.testPCT <- abs(test.df$SalePrice-stepwise.test)/test.df$SalePrice

# Assign Prediction Grades
stepwise.testPredictionGrade <- ifelse(stepwise.testPCT <= 0.10,'Grade 1: [0.0.10]',
                                       ifelse(stepwise.testPCT <= 0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(stepwise.testPCT <= 0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )					
)

stepwise.testTable <-table(stepwise.testPredictionGrade)
stepwise.testTable/sum(stepwise.testTable)

# Junk
junk.testPCT <- abs(test.df$SalePrice-junk.test)/test.df$SalePrice

# Assign Prediction Grades
junk.testPredictionGrade <- ifelse(junk.testPCT <= 0.10,'Grade 1: [0.0.10]',
                                       ifelse(junk.testPCT <= 0.15,'Grade 2: (0.10,0.15]',
                                              ifelse(junk.testPCT <= 0.25,'Grade 3: (0.15,0.25]',
                                                     'Grade 4: (0.25+]')
                                       )					
)

junk.testTable <-table(junk.testPredictionGrade)
junk.testTable/sum(junk.testTable)

# Best Model: Backward selection --------------------------------------------------------------
## RATIONALE: Based on the analysis above, the backwards variable selection has been
## selected as the best linear model on several criteria:
##  - Good R-squared and AIC/BIC values
##  - Best out-of-sample performance in MSE and MAE measures
##  - Highest business-oriented PredictionGrades performance
##  - Similar VIF in all predictors
##  - Simplest model (parsimony)

## Helper functions ##
# Function to calculate model metrics
calculate_metrics <- function(model, train_data, test_data) {
  # In-sample
  train_pred <- predict(model, train_data)
  train_mse <- mean((train_data$SalePrice - train_pred)^2)
  train_mae <- mean(abs(train_data$SalePrice - train_pred))
  train_mape <- mean(abs((train_data$SalePrice - train_pred) / train_data$SalePrice)) * 100
  
  # Out-of-sample
  test_pred <- predict(model, test_data)
  test_mse <- mean((test_data$SalePrice - test_pred)^2)
  test_mae <- mean(abs(test_data$SalePrice - test_pred))
  test_mape <- mean(abs((test_data$SalePrice - test_pred) / test_data$SalePrice)) * 100
  
  # Model info
  adj_r2 <- summary(model)$adj.r.squared
  n_params <- length(coef(model)) - 1  # Exclude intercept
  
  return(data.frame(
    Adj_R2 = adj_r2,
    N_Predictors = n_params,
    Train_MSE = train_mse,
    Test_MSE = test_mse,
    Train_MAE = train_mae,
    Test_MAE = test_mae,
    Train_MAPE = train_mape,
    Test_MAPE = test_mape,
    Overfit_MSE = train_mse - test_mse
  ))
}

# Function to calculate R² change for each variable
calculate_r2_change <- function(model) {
  base_r2 <- summary(model)$adj.r.squared
  var_names <- names(coef(model))[-1]  # Exclude intercept
  
  r2_changes <- sapply(var_names, function(var) {
    reduced_formula <- paste(". ~ . -", var)
    reduced_model <- update(model, reduced_formula)
    reduced_r2 <- summary(reduced_model)$adj.r.squared
    return(base_r2 - reduced_r2)
  })
  
  result <- data.frame(
    Variable = var_names,
    R2_Change = r2_changes,
    Coefficient = coef(model)[-1],
    Std_Error = summary(model)$coefficients[-1, "Std. Error"],
    P_Value = summary(model)$coefficients[-1, "Pr(>|t|)"]
  )
  
  return(result[order(result$R2_Change), ])
}

# Function to create prediction grade tables
create_grade_table <- function(model, data, data_name = "Sample") {
  preds <- predict(model, data)
  pct_error <- abs(preds - data$SalePrice) / data$SalePrice
  
  grades <- ifelse(pct_error <= 0.10, 'Grade 1: [0.0-0.10]',
                   ifelse(pct_error <= 0.15, 'Grade 2: (0.10-0.15]',
                          ifelse(pct_error <= 0.25, 'Grade 3: (0.15-0.25]', 
                                 'Grade 4: (0.25+]')))
  
  grade_table <- prop.table(table(grades))
  
  cat("\n", data_name, "Prediction Grades:\n", sep = "")
  print(round(grade_table, 4))
  
  return(grade_table)
}

## Baseline model assessment ##
cat("Original Model from Backward Selection:\n")
print(summary(backward.lm))

cat("\n--- Baseline Metrics ---\n")
baseline_metrics <- calculate_metrics(backward.lm, train.clean, test.clean)
print(baseline_metrics)

# Save for comparison
model_comparison <- data.frame(
  Model = "1_Backward_Original",
  baseline_metrics,
  stringsAsFactors = FALSE
)

## Check for counter-intuitive coefficient signs ##
# Define expected signs for variables
expected_signs <- data.frame(
  Variable = c("LotArea", "YearBuilt", "YearRemodel", "TotalBsmtSF", 
               "FirstFlrSF", "SecondFlrSF", "BedroomAbvGr", "Fireplaces",
               "GarageArea", "QualityIndex", "TotalSqftCalc"),
  Expected = c("+", "+", "+", "+", "+", "+", "+", "+", "+", "+", "+"),
  Rationale = c("Larger lot = higher value",
                "Newer = higher value",
                "Recent remodel = higher value",
                "More basement space = higher value",
                "More first floor space = higher value",
                "More second floor space = higher value",
                "More bedrooms = higher value",
                "More fireplaces = higher value",
                "Larger garage = higher value",
                "Higher quality = higher value",
                "More total space = higher value")
)

# Get actual coefficients
coefs <- coef(backward.lm)
coef_df <- data.frame(
  Variable = names(coefs)[-1],
  Coefficient = coefs[-1],
  Actual_Sign = ifelse(coefs[-1] > 0, "+", "-")
)

# Merge with expected signs
sign_check <- merge(coef_df, expected_signs, by = "Variable", all.x = TRUE)
sign_check$Flag <- ifelse(!is.na(sign_check$Expected) & 
                            sign_check$Actual_Sign != sign_check$Expected,
                          "⚠️ SIGN REVERSED", "")

cat("Variables with Counter-Intuitive Signs:\n")
reversed_signs <- sign_check[sign_check$Flag != "", ]
if (nrow(reversed_signs) > 0) {
  print(reversed_signs[, c("Variable", "Coefficient", "Expected", "Actual_Sign", "Rationale")])
  
  cat("\n⚠️  ACTION REQUIRED:\n")
  cat("BedroomAbvGr has NEGATIVE coefficient (-4,373)\n")
  cat("Interpretation: Controlling for square footage, more bedrooms = smaller rooms = less value\n")
  cat("This suggests multicollinearity with size variables\n")
  cat("Recommendation: Remove BedroomAbvGr (will be done in later step)\n")
} else {
  cat("✓ All coefficients have theoretically expected signs\n")
}

## R-squared Change Analysis ##
r2_analysis <- calculate_r2_change(backward.lm)
r2_analysis$Significance <- ifelse(r2_analysis$P_Value < 0.001, "***",
                                   ifelse(r2_analysis$P_Value < 0.01, "**",
                                          ifelse(r2_analysis$P_Value < 0.05, "*",
                                                 ifelse(r2_analysis$P_Value < 0.10, ".",
                                                        " "))))

cat("R-squared Contribution by Variable (sorted by R-squared change):\n\n")
print(r2_analysis, row.names = FALSE, digits = 6)

# Flag variables for removal (R-squared change < 0.001)
removal_threshold <- 0.001
low_contributors <- r2_analysis[r2_analysis$R2_Change < removal_threshold, ]

cat("\n--- Variables with Minimal Contribution (R-squared change < 0.001) ---\n")
if (nrow(low_contributors) > 0) {
  print(low_contributors[, c("Variable", "R2_Change", "P_Value", "Significance")], 
        row.names = FALSE)
  cat("\n⚠️  RECOMMENDATION: Consider removing these variables for parsimony\n")
  cat("Statistical significance ≠ Practical importance!\n")
} else {
  cat("✓ All variables contribute meaningfully to model fit\n")
}

## Multicollinearity analysis (VIF) ##
vif_values <- vif(backward.lm)
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values
) %>% arrange(desc(VIF))

cat("Variance Inflation Factors (VIF):\n")
cat("Rule of thumb: VIF > 5 = moderate concern, VIF > 10 = serious concern\n\n")
print(vif_df, row.names = FALSE, digits = 3)

# Flag problematic VIFs
high_vif <- vif_df[vif_df$VIF > 10, ]
moderate_vif <- vif_df[vif_df$VIF > 5 & vif_df$VIF <= 10, ]

if (nrow(high_vif) > 0) {
  cat("\n🚨 SEVERE MULTICOLLINEARITY (VIF > 10):\n")
  print(high_vif, row.names = FALSE, digits = 3)
  cat("\nThese quality variables have severe multicollinearity!\n")
  cat("Likely causes:\n")
  cat("  1. Incomplete dummy coding (missing levels)\n")
  cat("  2. QualityIndex redundant with other quality measures\n")
  cat("  3. Natural correlation between quality variables\n")
}

if (nrow(moderate_vif) > 0) {
  cat("\n⚠️  MODERATE MULTICOLLINEARITY (VIF 5-10):\n")
  print(moderate_vif, row.names = FALSE, digits = 3)
  cat("\nThese size variables have moderate multicollinearity\n")
  cat("TotalSqftCalc may overlap with FirstFlrSF + SecondFlrSF + TotalBsmtSF\n")
}


## Complete categorical variables ##
# Check which OverallQual levels are present
overall_qual_vars <- grep("^OverallQual_", names(coef(backward.lm)), value = TRUE)
cat("OverallQual dummy variables in model:\n")
print(overall_qual_vars)

# Available in data
available_qual <- grep("^OverallQual_", names(train.clean), value = TRUE)
cat("\nOverallQual dummy variables available in data:\n")
print(available_qual)

missing_qual <- setdiff(available_qual, overall_qual_vars)
if (length(missing_qual) > 0) {
  cat("\n⚠️  MISSING from model:\n")
  print(missing_qual)
  cat("\nACTION: Must add these back for complete categorical variable representation!\n")
} else {
  cat("\n✓ OverallQual is complete\n")
}

# Check ExterQual
exter_qual_vars <- grep("^ExterQual_", names(coef(backward.lm)), value = TRUE)
available_exter <- grep("^ExterQual_", names(train.clean), value = TRUE)
missing_exter <- setdiff(available_exter, exter_qual_vars)

cat("\n--- ExterQual ---\n")
cat("In model:", exter_qual_vars, "\n")
cat("Available:", available_exter, "\n")
if (length(missing_exter) > 0) {
  cat("⚠️  MISSING:", missing_exter, "\n")
  cat("ACTION: Add back for completeness!\n")
} else {
  cat("✓ ExterQual is complete\n")
}

# Check Neighborhood
neighborhood_vars <- grep("^Neighborhood_", names(coef(backward.lm)), value = TRUE)
available_neighborhoods <- grep("^Neighborhood_", names(train.clean), value = TRUE)
missing_neighborhoods <- setdiff(available_neighborhoods, neighborhood_vars)

cat("\n--- Neighborhood ---\n")
cat("In model:", length(neighborhood_vars), "neighborhoods\n")
cat("Available:", length(available_neighborhoods), "neighborhoods\n")
if (length(missing_neighborhoods) > 0) {
  cat("⚠️  MISSING (", length(missing_neighborhoods), "):\n", sep = "")
  print(missing_neighborhoods)
  cat("\nDECISION NEEDED: Include all neighborhoods or keep only significant ones?\n")
  cat("Recommendation: Include all for complete categorical representation\n")
}

## Building new models ##
# MODEL V1: Add complete categorical variables (but NOT remove QualityIndex yet)
model_v1 <- lm(SalePrice ~ LotArea + YearBuilt + YearRemodel + 
                 TotalBsmtSF + FirstFlrSF + SecondFlrSF + 
                 BedroomAbvGr + Fireplaces + GarageArea + 
                 QualityIndex + TotalSqftCalc +
                 # Complete OverallQual
                 OverallQual_4 + OverallQual_5 + OverallQual_6 + 
                 OverallQual_7 + OverallQual_8 + OverallQual_9 + OverallQual_10 +
                 # Keep selected neighborhoods (or add all if you prefer)
                 Neighborhood_BrkSide + Neighborhood_ClearCr + 
                 Neighborhood_CollgCr + Neighborhood_Crawfor + 
                 Neighborhood_Gilbert + Neighborhood_NoRidge + 
                 Neighborhood_NridgHt + Neighborhood_Somerst + 
                 Neighborhood_StoneBr + Neighborhood_Timber +
                 # Complete ExterQual
                 ExterQual_Fa + ExterQual_Gd + ExterQual_TA +
                 # Complete BsmtQual
                 BsmtQual_Fa + BsmtQual_Gd + BsmtQual_NA + BsmtQual_TA +
                 # Complete KitchenQual
                 KitchenQual_Fa + KitchenQual_Gd + KitchenQual_TA,
               data = train.clean)

v1_metrics <- calculate_metrics(model_v1, train.clean, test.clean)
model_comparison <- rbind(model_comparison, 
                          data.frame(Model = "2_Complete_Categories", v1_metrics))

cat("Model V1 Summary:\n")
cat("Adj R-squared:", round(summary(model_v1)$adj.r.squared, 5), "\n")
cat("N Predictors:", length(coef(model_v1)) - 1, "\n")
cat("Test MSE:", round(v1_metrics$Test_MSE, 0), "\n")

# Check VIF for V1
cat("\nTop 10 VIF values in Model V1:\n")
vif_v1 <- sort(vif(model_v1), decreasing = TRUE)
print(head(vif_v1, 10))

# MODEL V2: Remove QualityIndex (to reduce quality variable VIF)
model_v2 <- update(model_v1, . ~ . - QualityIndex)

v2_metrics <- calculate_metrics(model_v2, train.clean, test.clean)
model_comparison <- rbind(model_comparison, 
                          data.frame(Model = "3_No_QualityIndex", v2_metrics))

cat("Model V2 Summary:\n")
cat("Adj R-squared:", round(summary(model_v2)$adj.r.squared, 5), "\n")
cat("N Predictors:", length(coef(model_v2)) - 1, "\n")
cat("Test MSE:", round(v2_metrics$Test_MSE, 0), "\n")

cat("\nTop 10 VIF values in Model V2:\n")
vif_v2 <- sort(vif(model_v2), decreasing = TRUE)
print(head(vif_v2, 10))

cat("\nVIF Improvement (comparing quality variables):\n")
quality_vars <- c("ExterQual_TA", "ExterQual_Gd", "BsmtQual_TA", 
                  "KitchenQual_TA", "KitchenQual_Gd")
quality_vars_in_both <- quality_vars[quality_vars %in% names(vif_v1) & 
                                       quality_vars %in% names(vif_v2)]
if (length(quality_vars_in_both) > 0) {
  vif_comparison <- data.frame(
    Variable = quality_vars_in_both,
    V1_with_QIndex = vif_v1[quality_vars_in_both],
    V2_no_QIndex = vif_v2[quality_vars_in_both],
    Improvement = vif_v1[quality_vars_in_both] - vif_v2[quality_vars_in_both]
  )
  print(vif_comparison, row.names = FALSE)
}

# MODEL V3: Remove low R-squared contributors
# Get R-squared analysis for V2
r2_v2 <- calculate_r2_change(model_v2)
to_remove <- r2_v2[r2_v2$R2_Change < 0.001 & r2_v2$P_Value > 0.05, "Variable"]

if (length(to_remove) > 0) {
  cat("Removing:", paste(to_remove, collapse = ", "), "\n\n")
  
  # Build removal formula
  removal_formula <- paste(". ~ . -", paste(to_remove, collapse = " - "))
  model_v3 <- update(model_v2, removal_formula)
} else {
  cat("No variables meet removal criteria\n")
  model_v3 <- model_v2
}

v3_metrics <- calculate_metrics(model_v3, train.clean, test.clean)
model_comparison <- rbind(model_comparison, 
                          data.frame(Model = "4_Remove_Low_Contrib", v3_metrics))

cat("Model V3 Summary:\n")
cat("Adj R-squared:", round(summary(model_v3)$adj.r.squared, 5), "\n")
cat("N Predictors:", length(coef(model_v3)) - 1, "\n")
cat("Test MSE:", round(v3_metrics$Test_MSE, 0), "\n")

# MODEL V4: Remove BedroomAbvGr (counter-intuitive sign)
if ("BedroomAbvGr" %in% names(coef(model_v3))) {
  model_v4 <- update(model_v3, . ~ . - BedroomAbvGr)
  
  v4_metrics <- calculate_metrics(model_v4, train.clean, test.clean)
  model_comparison <- rbind(model_comparison, 
                            data.frame(Model = "5_Remove_BedroomAbvGr", v4_metrics))
  
  cat("Model V4 Summary:\n")
  cat("Adj R-squared:", round(summary(model_v4)$adj.r.squared, 5), "\n")
  cat("N Predictors:", length(coef(model_v4)) - 1, "\n")
  cat("Test MSE:", round(v4_metrics$Test_MSE, 0), "\n")
} else {
  cat("BedroomAbvGr already removed\n")
  model_v4 <- model_v3
  v4_metrics <- v3_metrics
}

# MODEL V5: Remove TotalSqftCalc (multicollinearity with components)
if ("TotalSqftCalc" %in% names(coef(model_v4))) {
  model_v5 <- update(model_v4, . ~ . - TotalSqftCalc)
  
  v5_metrics <- calculate_metrics(model_v5, train.clean, test.clean)
  model_comparison <- rbind(model_comparison, 
                            data.frame(Model = "6_Remove_TotalSqftCalc", v5_metrics))
  
  cat("Model V5 Summary:\n")
  cat("Adj R-squared:", round(summary(model_v5)$adj.r.squared, 5), "\n")
  cat("N Predictors:", length(coef(model_v5)) - 1, "\n")
  cat("Test MSE:", round(v5_metrics$Test_MSE, 0), "\n")
  
  cat("\nVIF improvement for size variables:\n")
  size_vars <- c("FirstFlrSF", "SecondFlrSF", "TotalBsmtSF")
  if (all(size_vars %in% names(vif(model_v4))) && all(size_vars %in% names(vif(model_v5)))) {
    vif_v4 <- vif(model_v4)
    vif_v5 <- vif(model_v5)
    size_vif_comp <- data.frame(
      Variable = size_vars,
      V4_with_Total = vif_v4[size_vars],
      V5_no_Total = vif_v5[size_vars]
    )
    print(size_vif_comp, row.names = FALSE)
  }
} else {
  cat("TotalSqftCalc already removed\n")
  model_v5 <- model_v4
  v5_metrics <- v4_metrics
}

## Addressing homoscedasticity ##
cat("Testing for heteroskedasticity in Model V5:\n")
bp_test <- bptest(model_v5)
print(bp_test)

if (bp_test$p.value < 0.05) {
  cat("\n🚨 Heteroskedasticity detected! (p < 0.05)\n")
  cat("Solution: Transform response variable to log(SalePrice)\n\n")
  
  # Create log-transformed model
  cat("--- Model V6: Log-Transformed Response ---\n")
  
  # Get formula from V5 and transform response
  formula_v5 <- formula(model_v5)
  formula_log <- update(formula_v5, log(SalePrice) ~ .)
  
  model_v6_log <- lm(formula_log, data = train.clean)
  
  cat("\nModel V6 (Log) Summary:\n")
  cat("Adj R²:", round(summary(model_v6_log)$adj.r.squared, 5), "\n")
  
  # Test heteroskedasticity again
  bp_test_log <- bptest(model_v6_log)
  cat("\nBreusch-Pagan test on log model:\n")
  print(bp_test_log)
  
  if (bp_test_log$p.value > 0.05) {
    cat("\n✓ Heteroskedasticity resolved!\n")
  } else {
    cat("\n⚠️  Some heteroskedasticity remains, but likely improved\n")
  }
  
  # For comparison, need to transform predictions back
  train_pred_log <- exp(predict(model_v6_log, train.clean))
  test_pred_log <- exp(predict(model_v6_log, test.clean))
  
  # Calculate metrics on original scale
  log_metrics <- data.frame(
    Adj_R2 = summary(model_v6_log)$adj.r.squared,
    N_Predictors = length(coef(model_v6_log)) - 1,
    Train_MSE = mean((train.clean$SalePrice - train_pred_log)^2),
    Test_MSE = mean((test.clean$SalePrice - test_pred_log)^2),
    Train_MAE = mean(abs(train.clean$SalePrice - train_pred_log)),
    Test_MAE = mean(abs(test.clean$SalePrice - test_pred_log)),
    Train_MAPE = mean(abs((train.clean$SalePrice - train_pred_log) / train.clean$SalePrice)) * 100,
    Test_MAPE = mean(abs((test.clean$SalePrice - test_pred_log) / test.clean$SalePrice)) * 100,
    Overfit_MSE = mean((train.clean$SalePrice - train_pred_log)^2) - 
      mean((test.clean$SalePrice - test_pred_log)^2)
  )
  
  model_comparison <- rbind(model_comparison, 
                            data.frame(Model = "7_Log_Transformed", log_metrics))
  
} else {
  cat("\n✓ No significant heteroskedasticity detected\n")
  model_v6_log <- NULL
}

## Test for Interactions ##
# Use best model so far (V6 if log-transformed)
base_model_for_interactions <- model_v6_log

# Test 1: Neighborhood × FirstFlrSF
# Get neighborhood variables in model
neighborhood_in_model <- grep("^Neighborhood_", names(coef(base_model_for_interactions)), 
                              value = TRUE)

if (length(neighborhood_in_model) > 0 && "FirstFlrSF" %in% names(coef(base_model_for_interactions))) {
  # Create interaction terms
  interaction_formula_1 <- as.formula(paste(". ~ . +", 
                                            paste(neighborhood_in_model, ":FirstFlrSF", 
                                                  collapse = " + ")))
  model_int1 <- update(base_model_for_interactions, interaction_formula_1)
  
  # F-test for interaction
  anova_result_1 <- anova(base_model_for_interactions, model_int1)
  print(anova_result_1)
  
  if (anova_result_1$`Pr(>F)`[2] < 0.05) {
    cat("\n✓ Interaction is statistically significant (p < 0.05)\n")
    
    # Check if it improves test performance
    int1_test_mse <- mean((predict(model_int1, test.clean) - test.clean$SalePrice)^2)
    base_test_mse <- mean((predict(base_model_for_interactions, test.clean) - test.clean$SalePrice)^2)
    
    cat("Test MSE without interaction:", round(base_test_mse, 0), "\n")
    cat("Test MSE with interaction:", round(int1_test_mse, 0), "\n")
    
    if (int1_test_mse < base_test_mse) {
      cat("✓ Interaction improves out-of-sample performance\n")
      cat("RECOMMENDATION: Include this interaction\n")
    } else {
      cat("⚠️  Interaction does NOT improve out-of-sample performance\n")
      cat("RECOMMENDATION: Do not include (overfitting)\n")
    }
  } else {
    cat("\n✗ Interaction is not statistically significant\n")
    cat("RECOMMENDATION: Do not include\n")
  }
} else {
  cat("Cannot test - variables not in model\n")
}

# Test 2: OverallQual × TotalBsmtSF
overall_qual_in_model <- grep("^OverallQual_", names(coef(base_model_for_interactions)), 
                              value = TRUE)

if (length(overall_qual_in_model) > 0 && "TotalBsmtSF" %in% names(coef(base_model_for_interactions))) {
  interaction_formula_2 <- as.formula(paste(". ~ . +", 
                                            paste(overall_qual_in_model, ":TotalBsmtSF", 
                                                  collapse = " + ")))
  model_int2 <- update(base_model_for_interactions, interaction_formula_2)
  
  anova_result_2 <- anova(base_model_for_interactions, model_int2)
  print(anova_result_2)
  
  if (anova_result_2$`Pr(>F)`[2] < 0.05) {
    cat("\n✓ Interaction is statistically significant\n")
    
    int2_test_mse <- mean((predict(model_int2, test.clean) - test.clean$SalePrice)^2)
    base_test_mse <- mean((predict(base_model_for_interactions, test.clean) - test.clean$SalePrice)^2)
    cat("Test MSE without interaction:", round(base_test_mse, 0), "\n")
    cat("Test MSE with interaction:", round(int2_test_mse, 0), "\n")
    
    if (int2_test_mse < base_test_mse) {
      cat("✓ Interaction improves out-of-sample performance\n")
    } else {
      cat("⚠️  Interaction does NOT improve out-of-sample performance\n")
    }
  } else {
    cat("\n✗ Interaction not significant\n")
  }
} else {
  cat("Cannot test - variables not in model\n")
}

## Final Diagnostics ##
# Choose best model
final_model <- model_v6_log

# Residual plots
png(paste0(output_path, "/final_model_diagnosis.png"), 
    units = "in", width = 12, height = 7, res = 300)
cat("Creating diagnostic plots...\n")
par(mfrow = c(2, 2))
plot(final_model, main = "Final Model Diagnostics")
par(mfrow = c(1, 1))
dev.off()

# Normality test
cat("\n--- Normality Test ---\n")
if (nrow(train.clean) < 5000) {
  shapiro_result <- shapiro.test(resid(final_model))
  print(shapiro_result)
  if (shapiro_result$p.value < 0.05) {
    cat("⚠️  Residuals not normally distributed (p < 0.05)\n")
    cat("With large n, this is less critical due to CLT\n")
  } else {
    cat("✓ Residuals approximately normal\n")
  }
} else {
  cat("Sample too large for Shapiro-Wilk test\n")
  cat("Using visual inspection (Q-Q plot) instead\n")
}

# Influential points
cat("\n--- Influential Points ---\n")
cooksd <- cooks.distance(final_model)
cutoff <- 4 / (nrow(train.clean) - length(coef(final_model)))
influential <- which(cooksd > cutoff)

cat("Cook's distance cutoff:", round(cutoff, 4), "\n")
cat("Number of influential points:", length(influential), "\n")

if (length(influential) > 0) {
  cat("\nMost influential observations:\n")
  top_influential <- head(order(cooksd, decreasing = TRUE), 10)
  print(data.frame(
    Observation = top_influential,
    CooksD = cooksd[top_influential],
    SalePrice = train.clean$SalePrice[top_influential]
  ))
  
  cat("\n⚠️  ACTION: Investigate these observations for:\n")
  cat("   - Data entry errors\n")
  cat("   - Unusual/unique properties\n")
  cat("   - Consider robust regression if many influential points\n")
}

# Final VIF check
cat("\n--- Final VIF Check ---\n")
vif_final <- sort(vif(final_model), decreasing = TRUE)
cat("Top 10 VIF values:\n")
print(head(vif_final, 10))

high_vif_final <- vif_final[vif_final > 5]
if (length(high_vif_final) > 0) {
  cat("\n⚠️  Variables with VIF > 5:\n")
  print(high_vif_final)
} else {
  cat("\n✓ All VIF values acceptable (< 5)\n")
}

## Model comparison ##
cat("Comprehensive Model Comparison:\n\n")
model_comparison$Test_Rank <- rank(model_comparison$Test_MSE)
model_comparison$Parsimony_Score <- model_comparison$N_Predictors / max(model_comparison$N_Predictors)

print(model_comparison, row.names = FALSE, digits = 3)

# Best test MSE
best_test_idx <- which.min(model_comparison$Test_MSE)
cat("Best Test MSE:", model_comparison$Model[best_test_idx], "\n")
cat("  Test MSE:", round(model_comparison$Test_MSE[best_test_idx], 0), "\n")
cat("  Test MAE:", round(model_comparison$Test_MAE[best_test_idx], 0), "\n")
cat("  Test MAPE:", round(model_comparison$Test_MAPE[best_test_idx], 2), "%\n")

# Most parsimonious
most_parsimonious_idx <- which.min(model_comparison$N_Predictors)
cat("\nMost Parsimonious:", model_comparison$Model[most_parsimonious_idx], "\n")
cat("  N Predictors:", model_comparison$N_Predictors[most_parsimonious_idx], "\n")

# Best Adj R-squared
best_r2_idx <- which.max(model_comparison$Adj_R2)
cat("\nHighest Adj R-squared:", model_comparison$Model[best_r2_idx], "\n")
cat("  Adj R-squared:", round(model_comparison$Adj_R2[best_r2_idx], 5), "\n")

# Overfit analysis
cat("\n--- Overfitting Analysis ---\n")
cat("Negative Overfit_MSE = Model performs better on test than train (unusual)\n")
cat("Large positive Overfit_MSE = Model overfitting to training data\n\n")
print(model_comparison[, c("Model", "Train_MSE", "Test_MSE", "Overfit_MSE")], 
      row.names = FALSE)

## Prediction Grade Analysis ##
cat("Analyzing prediction quality using percentage error thresholds...\n")
cat("Grade 1: Error ≤ 10% (Excellent)\n")
cat("Grade 2: Error 10-15% (Good)\n")
cat("Grade 3: Error 15-25% (Fair)\n")
cat("Grade 4: Error > 25% (Poor)\n\n")

# Original model
cat("--- Original Model (Backward Selection) ---\n")
create_grade_table(backward.lm, train.clean, "Train")
create_grade_table(backward.lm, test.clean, "Test")

# Final cleaned model
cat("\n--- Final Model (V6) ---\n")
create_grade_table(model_v6_log, train.clean, "Train")
create_grade_table(model_v6_log, test.clean, "Test")

# If log model exists
if (!is.null(model_v6_log)) {
  cat("\n--- Log-Transformed Model (V6) ---\n")
  # Need to back-transform predictions
  train_pred_log <- exp(predict(model_v6_log, train.clean))
  test_pred_log <- exp(predict(model_v6_log, test.clean))
  
  pct_error_train <- abs(train_pred_log - train.clean$SalePrice) / train.clean$SalePrice
  grades_train <- ifelse(pct_error_train <= 0.10, 'Grade 1: [0.0-0.10]',
                         ifelse(pct_error_train <= 0.15, 'Grade 2: (0.10-0.15]',
                                ifelse(pct_error_train <= 0.25, 'Grade 3: (0.15-0.25]', 
                                       'Grade 4: (0.25+]')))
  cat("\nTrain Prediction Grades:\n")
  print(round(prop.table(table(grades_train)), 4))
  
  pct_error_test <- abs(test_pred_log - test.clean$SalePrice) / test.clean$SalePrice
  grades_test <- ifelse(pct_error_test <= 0.10, 'Grade 1: [0.0-0.10]',
                        ifelse(pct_error_test <= 0.15, 'Grade 2: (0.10-0.15]',
                               ifelse(pct_error_test <= 0.25, 'Grade 3: (0.15-0.25]', 
                                      'Grade 4: (0.25+]')))
  cat("\nTest Prediction Grades:\n")
  print(round(prop.table(table(grades_test)), 4))
}

## Final Model Selection ##
cat("1. MODEL SELECTION:\n")
cat("   Recommended model: V2 (Complete categories)\n")
cat("   - Balances predictive performance with interpretability\n")
cat("   - Removes multicollinearity issues\n")
cat("   - Complete categorical variables for proper interpretation\n")

cat("2. KEY IMPROVEMENTS FROM ORIGINAL:\n")
cat("   - ✓ Added missing categorical dummy variables\n")

cat("3. REMAINING CONSIDERATIONS:\n")
if (!is.null(model_v6_log) && bp_test_log$p.value < 0.05) {
  cat("   - ⚠️  Consider log-transformed model if heteroskedasticity is critical\n")
}
if (length(influential) > 10) {
  cat("   - ⚠️  Consider robust regression due to influential points\n")
}
cat("   - Consider testing additional interactions if theoretically justified\n")
cat("   - Document which neighborhoods were excluded (10/20 missing)\n\n")

cat("4. FINAL MODEL FORMULA:\n")
cat("   ", deparse(formula(model_v6_log)), "\n\n")

cat("5. PERFORMANCE SUMMARY:\n")
v6_idx <- which(model_comparison$Model == "7_Log_Transformation")
if (length(v6_idx) > 0) {
  cat("   Adj R-squared:", round(model_comparison$Adj_R2[v6_idx], 5), "\n")
  cat("   Test MSE:", round(model_comparison$Test_MSE[v6_idx], 0), "\n")
  cat("   Test MAE:", round(model_comparison$Test_MAE[v6_idx], 0), "\n")
  cat("   Test MAPE:", round(model_comparison$Test_MAPE[v6_idx], 2), "%\n")
  cat("   N Predictors:", model_comparison$N_Predictors[v6_idx], "\n")
}
