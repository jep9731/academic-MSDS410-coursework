################################################################################
# MSDS 410 - Assignment 3: Multiple Linear Regression & Model Comparison
# Author: Joshua Pasaye
# Purpose: Demonstrate multiple regression modeling, nested model comparison,
#          and partial F-test for variable significance testing
# 
# KEY INSIGHTS DEMONSTRATED:
# 1. Multiple linear regression with correlated predictors
# 2. Interpretation of regression coefficients in multivariate context
# 3. Nested model comparison using partial F-test
# 4. Model selection based on statistical criteria (R², Adj R², AIC)
# 5. Understanding when additional predictors improve model fit
# 6. Translating complex regression output into actionable insights
################################################################################

# Set-up environment ------------------------------------------------------
# Remove everything from environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(readxl)
library(janitor)

# Load dataset
df <- read_xlsx("ames_housing_data.xlsx")

# Set-up model 3 (MLR) ----------------------------------------------------
# Use group variable set 1: GrLivArea, TotalBsmtSF, GarageArea with SalePrice
m1_vars <- c("GrLivArea", "TotalBsmtSF", "GarageArea", "SalePrice")

# Fit model
reduced_model <- lm(SalePrice ~ .,data = df[m1_vars])

# Summary table
summary(reduced_model)

# Omnibus ANOVA
anova(reduced_model)

# Set-up model 4 (Nested MLR) ----------------------------------------------------
# Add group variable set 3: BedroomAbvGr, FullBath, and OverallQual to create nested model
m2_vars <- c("GrLivArea", "TotalBsmtSF", "GarageArea", "SalePrice", "BedroomAbvGr", "FullBath", "OverallQual")

# Fit model
full_model <- lm(SalePrice ~ ., data = df[m2_vars])

# Summary table
summary(full_model)

# Omnibus
anova(full_model)

# Compute partial F-test
partial_f_test <- anova(reduced_model, full_model)

# Display the results
print(partial_f_test)
