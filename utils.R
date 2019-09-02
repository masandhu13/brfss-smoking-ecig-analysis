# Author: Mannat Sandhu
# July 14th, 2019
# BRFSS Data Analysis
# Utility functions used in the project.

# This function does the following - 
# 1. Loads SAS data. 
# 2. Filters the data based on the variables we need for the analysis.
# 3. Defines 2 new variables for 4 level e-cigarette and smoker statuses.
# 4. Adds the new variables the the filtered data.
# The function returns the filtered data.
GetCleanData <- function() {
  library(Hmisc)
  
  # Load SAS data.
  # Change the directory as per local machine.
  brfss_data = sasxport.get("~/Desktop/LLCP2017.XPT")
  
  # Write to CSV file for later (if needed) - 
  # write.csv(brfss, file = "LLCP2017.csv")
  
  variables_of_interest = c('x.state', 'income2', 'x.prace1', 'x.ageg5yr', 'sex', 'employ1', 'educa', 'ecigaret', 'ecignow', 'smoke100', 'smokday2')
  brfss_filtered = brfss_data[variables_of_interest]
  brfss_filtered = as.data.frame(brfss_filtered)
  
  # Calculate a four-level e-cigarette smoker status: 
  # 1. Everyday e-cigarette user 
  # 2. Someday e-cigarette user
  # 3. Former e-cigarette user
  # 4. Non-e-cigarette user
  # This is taken from Calculated Variables in 2017 BRFSS Data.
  # (https://www.cdc.gov/brfss/annual_data/2017/pdf/2017-calculated-variables-version4-508.pdf)
  ecigsts = ifelse(brfss_filtered$ecigaret == 2, 4, 
                   ifelse(brfss_filtered$ecigaret == 1, 
                          ifelse(brfss_filtered$ecignow == 1, 1,
                                 ifelse(brfss_filtered$ecignow == 2, 2,
                                        ifelse(brfss_filtered$ecignow == 3, 3, 9))),
                          9))
  
  brfss_filtered = cbind(brfss_filtered, ecigsts)
  
  # Calculate a four-level smoker status: 
  # 1. Everyday smoker
  # 2. Someday smoker
  # 3. Former smoker
  # 4. Non smoker
  # This is taken from Calculated Variables in 2017 BRFSS Data.
  # (https://www.cdc.gov/brfss/annual_data/2017/pdf/2017-calculated-variables-version4-508.pdf)
  smoker3 = ifelse(brfss_filtered$smoke100 == 2, 4, 
                   ifelse(brfss_filtered$smoke100 == 1, 
                          ifelse(brfss_filtered$smokday2 == 1, 1,
                                 ifelse(brfss_filtered$smokday2 == 2, 2,
                                        ifelse(brfss_filtered$smokday2 == 3, 3, 9))),
                          9))
  
  brfss_filtered = cbind(brfss_filtered, smoker3)
  return (brfss_filtered)
}

# Helper functions which remove the incomplete responses to the features.
FilterCompleteResponsesAge <- function(brfss_filtered) {
  brfss_filtered = brfss_filtered[brfss_filtered$x.ageg5yr != 14, ]
  return (brfss_filtered)
}

FilterCompleteResponsesRace <- function(brfss_filtered) {
  brfss_filtered = brfss_filtered[brfss_filtered$x.prace1 >=1 & brfss_filtered$x.prace1 <=7, ]
  return (brfss_filtered)
}

FilterCompleteResponsesSex <- function(brfss_filtered) {
  brfss_filtered = brfss_filtered[brfss_filtered$sex ==1 | brfss_filtered$sex ==2, ]
  return (brfss_filtered)
}

FilterCompleteResponsesEmployment <- function(brfss_filtered) {
  brfss_filtered = brfss_filtered[brfss_filtered$employ1 >=1 & brfss_filtered$employ1 <=8, ]
  return (brfss_filtered)
}

FilterCompleteResponsesEducation <- function(brfss_filtered) {
  brfss_filtered = brfss_filtered[brfss_filtered$educa >=1 & brfss_filtered$educa <=6, ]
  return (brfss_filtered)
}

FilterCompleteResponsesIncomeLevel <- function(brfss_filtered) {
  brfss_filtered = brfss_filtered[brfss_filtered$income2 >=1 & brfss_filtered$income2 <=8, ]
  return (brfss_filtered)
}

# This function takes as input clean brfss data (output of GetCleanData())
# and removes the incomplete responses to the features in the variable of interest.
FilterCompleteResponses <- function(brfss_filtered) {
  brfss_filtered = FilterCompleteResponsesAge(brfss_filtered) 
  brfss_filtered = FilterCompleteResponsesRace(brfss_filtered)
  brfss_filtered = FilterCompleteResponsesSex(brfss_filtered)
  brfss_filtered = FilterCompleteResponsesEmployment(brfss_filtered)
  brfss_filtered = FilterCompleteResponsesEducation(brfss_filtered)
  brfss_filtered = FilterCompleteResponsesIncomeLevel(brfss_filtered)
  return (brfss_filtered)
}

# Helper function used for GetExpConfidenceIntervalDataFrame() to get 
# string value in the desired format.
GetCIString <- function(value, low_ci, high_ci) {
  value <- sprintf("%0.2f", value)
  low_ci <- sprintf("%0.2f", low_ci)
  high_ci <- sprintf("%0.2f", high_ci)
  return (paste(value, " (", low_ci, ", ", high_ci, ")", sep = ""))  
}

# Returns the coefficients in the format - 
# "exp(value) (exp(2.5% CI Value), exp(97.5% CI Value))"
GetExpConfidenceIntervalDataFrame <- function(glm_model) {
  exp_coeff = exp(coef(glm_model))
  exp_coeff_ci = exp(confint.default(glm_model))
  
  exp_coeff <- as.data.frame(exp_coeff)
  exp_coeff_ci <- as.data.frame(exp_coeff_ci)
  exp_coeff <- cbind(exp_coeff, exp_coeff_ci$`2.5 %`, exp_coeff_ci$`97.5 %`)
  colnames(exp_coeff) <- c("value", "low_ci", "high_ci")
  
  exp_coeff$ci_string = GetCIString(exp_coeff$value, exp_coeff$low_ci, exp_coeff$high_ci)
  # Remove other columns.
  exp_coeff = exp_coeff[c('ci_string')]
  return (exp_coeff)
}

