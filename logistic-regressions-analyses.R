# Author: Mannat Sandhu
# July 14th, 2019
# BRFSS Data Analysis
# Part (2) - Coparing of smokers and e-cigarette users.

source("utils.R")

# Load data.
brfss_filtered = GetCleanData()
brfss_filtered = FilterCompleteResponses(brfss_filtered)

# Calculate a two-level status:
# 1. E-cigarette smoker - Everyday e-cigarette user and Someday e-cigarette user
# 2. Non E-cigarette smoker - Former e-cigarette user and Non-e-cigarette user
ecig_user = ifelse(brfss_filtered$ecigsts == 1 | brfss_filtered$ecigsts == 2, 1, 0)
brfss_filtered = cbind(brfss_filtered, ecig_user)

# Calculate a two-level status:
# 1. Smoker - Everyday smoker and Someday smoker
# 2. Non smoker - Former smoker and Non smoker
cig_user = ifelse(brfss_filtered$smoker3 == 1 | brfss_filtered$smoker3 == 2, 1, 0)
brfss_filtered = cbind(brfss_filtered, cig_user)


# CASE 1 - Run logistic regression for e-cigarette user/non-user.
# Generate a new dataset used for user/non-user logistic regression on e-cigarette users.
df_ecig_user_non_user = brfss_filtered[brfss_filtered$ecig_user == 1 | brfss_filtered$ecigsts == 4, ]
ecig_user_non_user = ifelse(df_ecig_user_non_user$ecig_user == 1, TRUE, FALSE)
df_ecig_user_non_user = cbind(df_ecig_user_non_user, ecig_user_non_user)
rm(ecig_user_non_user)

# Run logistic regression.
logit_ecig_user_non_user <- glm(ecig_user_non_user ~  as.factor(x.ageg5yr) + as.factor(x.prace1) + as.factor(sex) + as.factor(income2) + as.factor(educa) + as.factor(employ1), data = df_ecig_user_non_user, family = "binomial")
summary(logit_ecig_user_non_user)
exp_coeff = GetExpConfidenceIntervalDataFrame(logit_ecig_user_non_user)
write.csv(exp_coeff, file = "exp_coeff_ecig_non_ecig.csv")
rm(df_ecig_user_non_user)
########################################################################

# CASE 2 - Run logistic regression for e-cigarette user/former user.
df_ecig_user_former_user = brfss_filtered[brfss_filtered$ecig_user == 1 | brfss_filtered$ecigsts == 3, ]
ecig_user_former_user = ifelse(df_ecig_user_former_user$ecig_user == 1, TRUE, FALSE)
df_ecig_user_former_user = cbind(df_ecig_user_former_user, ecig_user_former_user)
rm(ecig_user_former_user)

# Run logistic regression.
logit_ecig_user_former_user <- glm(ecig_user_former_user ~  as.factor(x.ageg5yr) + as.factor(x.prace1) + as.factor(sex) + as.factor(income2) + as.factor(educa) + as.factor(employ1), data = df_ecig_user_former_user, family = "binomial")
summary(logit_ecig_user_former_user)
exp_coeff = GetExpConfidenceIntervalDataFrame(logit_ecig_user_former_user)
write.csv(exp_coeff, file = "exp_coeff_ecig_former_ecig.csv")
rm(df_ecig_user_former_user)
########################################################################

# CASE 3 - Run logistic regression for smoker/non-smoker.
# Generate a new dataset used for user/non-user logistic regression on smokers.
df_cig_user_non_user = brfss_filtered[brfss_filtered$cig_user == 1 | brfss_filtered$smoker3 == 4, ]
cig_user_non_user = ifelse(df_cig_user_non_user$cig_user == 1, TRUE, FALSE)
df_cig_user_non_user = cbind(df_cig_user_non_user, cig_user_non_user)
rm(cig_user_non_user)

# Run logistic regression.
logit_cig_user_non_user <- glm(cig_user_non_user ~ as.factor(x.ageg5yr) + as.factor(x.prace1) + as.factor(sex) + as.factor(income2) + as.factor(educa) + as.factor(employ1), data = df_cig_user_non_user, family = "binomial")
summary(logit_cig_user_non_user)
exp_coeff = GetExpConfidenceIntervalDataFrame(logit_cig_user_non_user)
write.csv(exp_coeff, file = "exp_coeff_smoker_non_smoker.csv")
rm(df_cig_user_non_user)
########################################################################

# CASE 4 - Run logistic regression for smoker/former smoker.
df_cig_user_former_user = brfss_filtered[brfss_filtered$cig_user == 1 | brfss_filtered$smoker3 == 3, ]
cig_user_former_user = ifelse(df_cig_user_former_user$cig_user == 1, TRUE, FALSE)
df_cig_user_former_user = cbind(df_cig_user_former_user, cig_user_former_user)
rm(cig_user_former_user)

# Run logistic regression.
logit_cig_user_former_user <- glm(cig_user_former_user ~  as.factor(x.ageg5yr) + as.factor(x.prace1) + as.factor(sex) + as.factor(income2) + as.factor(educa) + as.factor(employ1), data = df_cig_user_former_user, family = "binomial")
summary(logit_cig_user_former_user)
exp_coeff = GetExpConfidenceIntervalDataFrame(logit_cig_user_former_user)
write.csv(exp_coeff, file = "exp_coeff_cig_former_cig.csv")
rm(df_cig_user_former_user)
########################################################################



