# Author: Mannat Sandhu
# July 14th, 2019
# BRFSS Data Analysis
# Part (1) - Prevalence and Demographics of smokers and e-cigarette users.

source("utils.R")

library(Hmisc)
library(ggplot2)
library(scales)
library(grid)

brfss_filtered = GetCleanData()

# CASE 1 - Patterns across age groups.
# Copy data for processing e-cigarette use.
df_ecig = brfss_filtered
df_ecig = df_ecig[df_ecig$ecigsts == 1 | df_ecig$ecigsts == 2 | df_ecig$ecigsts == 3 | df_ecig$ecigsts == 4, ]
df_ecig = df_ecig[c('x.ageg5yr', 'ecigsts')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_ecig$ecigsts == 1, 1, 0)
cur_non_daily_user = ifelse(df_ecig$ecigsts == 2, 1, 0)

# Attach to the ecig data frame.
df_ecig = cbind(df_ecig, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out respondents who do have x.ageg5yr = 14 (Don’t know/Refused/Missing or not an adult)
df_ecig = df_ecig[complete.cases(df_ecig[ , c('x.ageg5yr')]), ]
df_ecig = FilterCompleteResponsesAge(df_ecig)

# Plot the data.
p1 <- ggplot() +
  stat_summary(data=df_ecig, aes(x = factor(x.ageg5yr), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_ecig, aes(x = factor(x.ageg5yr), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_ecig, aes(x = factor(x.ageg5yr), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_ecig, aes(x = factor(x.ageg5yr), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Age") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of e-cigarette users among U.S. adults, according to age group, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('18−24', '25−29', '30−34', '35−39', '40−44', '45−49', '50−54', '55−59', '60−64', '65−69', '70−74', '75-79', '>=80')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))


# Copy data for processing smoker use.
df_smoker = brfss_filtered
df_smoker = df_smoker[df_smoker$smoker3 == 1 | df_smoker$smoker3 == 2 | df_smoker$smoker3 == 3 |df_smoker$smoker3 == 4, ]
df_smoker = df_smoker[c('x.ageg5yr', 'smoker3')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_smoker$smoker3 == 1, 1, 0)
cur_non_daily_user = ifelse(df_smoker$smoker3 == 2, 1, 0)

# Attach to the ecig data frame.
df_smoker = cbind(df_smoker, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out respondents who do have x.ageg5yr = 14 (Don’t know/Refused/Missing or not an adult)
df_smoker = df_smoker[complete.cases(df_smoker[ , c('x.ageg5yr')]), ]
df_smoker = FilterCompleteResponsesAge(df_smoker)

# Plot the data.
p2 <- ggplot() +
  stat_summary(data=df_smoker, aes(x = factor(x.ageg5yr), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_smoker, aes(x = factor(x.ageg5yr), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_smoker, aes(x = factor(x.ageg5yr), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_smoker, aes(x = factor(x.ageg5yr), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Age") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of smokers among U.S. adults, according to age group, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('18−24', '25−29', '30−34', '35−39', '40−44', '45−49', '50−54', '55−59', '60−64', '65−69', '70−74', '75-79', '>=80')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

rm(df_ecig, df_smoker)
########################################################################

# CASE 2 - Patterns across Race.
# Copy data for processing e-cigarette use.
df_ecig = brfss_filtered
df_ecig = df_ecig[df_ecig$ecigsts == 1 | df_ecig$ecigsts == 2 | df_ecig$ecigsts == 3 |df_ecig$ecigsts == 4, ]
df_ecig = df_ecig[c('x.prace1', 'ecigsts')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_ecig$ecigsts == 1, 1, 0)
cur_non_daily_user = ifelse(df_ecig$ecigsts == 2, 1, 0)

# Attach to the ecig data frame.
df_ecig = cbind(df_ecig, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA values.
df_ecig = df_ecig[complete.cases(df_ecig[ , c('x.prace1')]), ]
# Filter out respondents who do have x.race not in {1, 2, ..., 7} (refused/unknown)
df_ecig = FilterCompleteResponsesRace(df_ecig)

# Plot the data.
p1 <- ggplot() +
  stat_summary(data=df_ecig, aes(x = factor(x.prace1), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_ecig, aes(x = factor(x.prace1), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_ecig, aes(x = factor(x.prace1), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_ecig, aes(x = factor(x.prace1), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Race") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of e-cigarette users among U.S. adults, according to race, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('White', 'Black', 'American Indian', 'Asian', 'Hawaiian/Pacific Islander', 'Other', 'No Preferred')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))


# Copy data for processing smoker use.
df_smoker = brfss_filtered
df_smoker = df_smoker[df_smoker$smoker3 == 1 | df_smoker$smoker3 == 2 | df_smoker$smoker3 == 3 |df_smoker$smoker3 == 4, ]
df_smoker = df_smoker[c('x.prace1', 'smoker3')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_smoker$smoker3 == 1, 1, 0)
cur_non_daily_user = ifelse(df_smoker$smoker3 == 2, 1, 0)

# Attach to the ecig data frame.
df_smoker = cbind(df_smoker, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA values.
df_smoker = df_smoker[complete.cases(df_smoker[ , c('x.prace1')]), ]
# Filter out respondents who do have x.race not in {1, 2, ..., 7} (refused/unknown)
df_smoker = FilterCompleteResponsesRace(df_smoker)

# Plot the data.
p2 <- ggplot() +
  stat_summary(data=df_smoker, aes(x = factor(x.prace1), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_smoker, aes(x = factor(x.prace1), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_smoker, aes(x = factor(x.prace1), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_smoker, aes(x = factor(x.prace1), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Race") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of smokers among U.S. adults, according to race, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('White', 'Black', 'American Indian', 'Asian', 'Hawaiian/Pacific Islander', 'Other', 'No Preferred')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

rm(df_ecig, df_smoker)
########################################################################


# CASE 3 - Patterns across Sex.
# Copy data for processing e-cigarette use.
df_ecig = brfss_filtered
df_ecig = df_ecig[df_ecig$ecigsts == 1 | df_ecig$ecigsts == 2 | df_ecig$ecigsts == 3 |df_ecig$ecigsts == 4, ]
df_ecig = df_ecig[c('sex', 'ecigsts')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_ecig$ecigsts == 1, 1, 0)
cur_non_daily_user = ifelse(df_ecig$ecigsts == 2, 1, 0)

# Attach to the ecig data frame.
df_ecig = cbind(df_ecig, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA and incoplete values.
df_ecig = df_ecig[complete.cases(df_ecig[ , c('sex')]), ]
df_ecig = FilterCompleteResponsesSex(df_ecig)

# Plot the data.
p1 <- ggplot() +
  stat_summary(data=df_ecig, aes(x = factor(sex), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_ecig, aes(x = factor(sex), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_ecig, aes(x = factor(sex), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_ecig, aes(x = factor(sex), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Sex") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of e-cigarette users among U.S. adults, according to sex, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('Male', 'Female')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))


# Copy data for processing smoker use.
df_smoker = brfss_filtered
df_smoker = df_smoker[df_smoker$smoker3 == 1 | df_smoker$smoker3 == 2 | df_smoker$smoker3 == 3 |df_smoker$smoker3 == 4, ]
df_smoker = df_smoker[c('sex', 'smoker3')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_smoker$smoker3 == 1, 1, 0)
cur_non_daily_user = ifelse(df_smoker$smoker3 == 2, 1, 0)

# Attach to the ecig data frame.
df_smoker = cbind(df_smoker, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA and incoplete values.
df_smoker = df_smoker[complete.cases(df_smoker[ , c('sex')]), ]
df_smoker = FilterCompleteResponsesSex(df_smoker)

# Plot the data.
p2 <- ggplot() +
  stat_summary(data=df_smoker, aes(x = factor(sex), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_smoker, aes(x = factor(sex), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_smoker, aes(x = factor(sex), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_smoker, aes(x = factor(sex), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Sex") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of smokers among U.S. adults, according to sex, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('Male', 'Female')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

rm(df_ecig, df_smoker)
########################################################################

# CASE 4 - Patterns across Employment Status.
# Copy data for processing e-cigarette use.
df_ecig = brfss_filtered
df_ecig = df_ecig[df_ecig$ecigsts == 1 | df_ecig$ecigsts == 2 | df_ecig$ecigsts == 3 |df_ecig$ecigsts == 4, ]
df_ecig = df_ecig[c('employ1', 'ecigsts')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_ecig$ecigsts == 1, 1, 0)
cur_non_daily_user = ifelse(df_ecig$ecigsts == 2, 1, 0)

# Attach to the ecig data frame.
df_ecig = cbind(df_ecig, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA and incoplete values.
df_ecig = df_ecig[complete.cases(df_ecig[ , c('employ1')]), ]
df_ecig = FilterCompleteResponsesEmployment(df_ecig)

# Plot the data.
p1 <- ggplot() +
  stat_summary(data=df_ecig, aes(x = factor(employ1), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_ecig, aes(x = factor(employ1), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_ecig, aes(x = factor(employ1), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_ecig, aes(x = factor(employ1), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Employment Status") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of e-cigarette users among U.S. adults, according to employment status, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('Employed\n for wages', 'Self-employed', 'Out of work\n >= 1 year', 'Out of work\n < 1 year', 'Homemaker', 'Student', 'Retired', 'Unable\n to work')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))


# Copy data for processing smoker use.
df_smoker = brfss_filtered
df_smoker = df_smoker[df_smoker$smoker3 == 1 | df_smoker$smoker3 == 2 | df_smoker$smoker3 == 3 |df_smoker$smoker3 == 4, ]
df_smoker = df_smoker[c('employ1', 'smoker3')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_smoker$smoker3 == 1, 1, 0)
cur_non_daily_user = ifelse(df_smoker$smoker3 == 2, 1, 0)

# Attach to the ecig data frame.
df_smoker = cbind(df_smoker, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA and incoplete values.
df_smoker = df_smoker[complete.cases(df_smoker[ , c('employ1')]), ]
df_smoker = FilterCompleteResponsesEmployment(df_smoker)

# Plot the data.
p2 <- ggplot() +
  stat_summary(data=df_smoker, aes(x = factor(employ1), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_smoker, aes(x = factor(employ1), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_smoker, aes(x = factor(employ1), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_smoker, aes(x = factor(employ1), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Employment Status") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of smokers among U.S. adults, according to employment status, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('Employed\n for wages', 'Self-employed', 'Out of work\n >= 1 year', 'Out of work\n < 1 year', 'Homemaker', 'Student', 'Retired', 'Unable\n to work')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

rm(df_ecig, df_smoker)
########################################################################


# CASE 5 - Patterns across Highest Education Level.
# Copy data for processing e-cigarette use.
df_ecig = brfss_filtered
df_ecig = df_ecig[df_ecig$ecigsts == 1 | df_ecig$ecigsts == 2 | df_ecig$ecigsts == 3 |df_ecig$ecigsts == 4, ]
df_ecig = df_ecig[c('educa', 'ecigsts')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_ecig$ecigsts == 1, 1, 0)
cur_non_daily_user = ifelse(df_ecig$ecigsts == 2, 1, 0)

# Attach to the ecig data frame.
df_ecig = cbind(df_ecig, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA and incoplete values.
df_ecig = df_ecig[complete.cases(df_ecig[ , c('educa')]), ]
df_ecig = FilterCompleteResponsesEducation(df_ecig)

# Plot the data.
p1 <- ggplot() +
  stat_summary(data=df_ecig, aes(x = factor(educa), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_ecig, aes(x = factor(educa), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_ecig, aes(x = factor(educa), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_ecig, aes(x = factor(educa), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Education Level") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of e-cigarette users among U.S. adults, according to highest education level, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('Never\n attended school\n or only\n kindergarten', 'Elementary\n (Grade 1-8)', 'Some\n high school\n (Grade 9-11)', 'High school\n graduate\n (Grade 12/GED)', 'Some college\n or technical\n school\n (College\n 1-3 years)', 'College\n graduate\n (College\n >= 4 years)')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))


# Copy data for processing smoker use.
df_smoker = brfss_filtered
df_smoker = df_smoker[df_smoker$smoker3 == 1 | df_smoker$smoker3 == 2 | df_smoker$smoker3 == 3 |df_smoker$smoker3 == 4, ]
df_smoker = df_smoker[c('educa', 'smoker3')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_smoker$smoker3 == 1, 1, 0)
cur_non_daily_user = ifelse(df_smoker$smoker3 == 2, 1, 0)

# Attach to the ecig data frame.
df_smoker = cbind(df_smoker, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA and incoplete values.
df_smoker = df_smoker[complete.cases(df_smoker[ , c('educa')]), ]
df_smoker = FilterCompleteResponsesEducation(df_smoker)

# Plot the data.
p2 <- ggplot() +
  stat_summary(data=df_smoker, aes(x = factor(educa), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_smoker, aes(x = factor(educa), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_smoker, aes(x = factor(educa), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_smoker, aes(x = factor(educa), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Education Level") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of smokers among U.S. adults, according to highest education level, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('Never\n attended school\n or only\n kindergarten', 'Elementary\n (Grade 1-8)', 'Some\n high school\n (Grade 9-11)', 'High school\n graduate\n (Grade 12/GED)', 'Some college\n or technical\n school\n (College\n 1-3 years)', 'College\n graduate\n (College\n >= 4 years)')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

rm(df_ecig, df_smoker)
########################################################################

# CASE 6 - Patterns across income level.
# Copy data for processing e-cigarette use.
df_ecig = brfss_filtered
df_ecig = df_ecig[df_ecig$ecigsts == 1 | df_ecig$ecigsts == 2 | df_ecig$ecigsts == 3 |df_ecig$ecigsts == 4, ]
df_ecig = df_ecig[c('income2', 'ecigsts')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_ecig$ecigsts == 1, 1, 0)
cur_non_daily_user = ifelse(df_ecig$ecigsts == 2, 1, 0)

# Attach to the ecig data frame.
df_ecig = cbind(df_ecig, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA and incoplete values.
df_ecig = df_ecig[complete.cases(df_ecig[ , c('income2')]), ]
df_ecig = FilterCompleteResponsesIncomeLevel(df_ecig)

# Plot the data.
p1 <- ggplot() +
  stat_summary(data=df_ecig, aes(x = factor(income2), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_ecig, aes(x = factor(income2), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_ecig, aes(x = factor(income2), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_ecig, aes(x = factor(income2), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Income Level") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of e-cigarette users among U.S. adults, according to income level, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('<$10,000', '$10,000\n-\n$15,000', '$15,000\n-\n$20,000', '$20,000\n-\n$25,000', '$25,000\n-\n$35,000', '$35,000\n-\n$50,000', '$50,000\n-\n$75,000', '>=$75,000')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))


# Copy data for processing smoker use.
df_smoker = brfss_filtered
df_smoker = df_smoker[df_smoker$smoker3 == 1 | df_smoker$smoker3 == 2 | df_smoker$smoker3 == 3 |df_smoker$smoker3 == 4, ]
df_smoker = df_smoker[c('income2', 'smoker3')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_smoker$smoker3 == 1, 1, 0)
cur_non_daily_user = ifelse(df_smoker$smoker3 == 2, 1, 0)

# Attach to the ecig data frame.
df_smoker = cbind(df_smoker, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA and incoplete values.
df_smoker = df_smoker[complete.cases(df_smoker[ , c('income2')]), ]
df_smoker = FilterCompleteResponsesIncomeLevel(df_smoker)

# Plot the data.
p2 <- ggplot() +
  stat_summary(data=df_smoker, aes(x = factor(income2), y = cur_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(data=df_smoker, aes(x = factor(income2), y = cur_daily_user, col="Current daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  stat_summary(data=df_smoker, aes(x = factor(income2), y = cur_non_daily_user), fun.data=mean_cl_boot, geom="errorbar", width=0.2) + 
  stat_summary(data=df_smoker, aes(x = factor(income2), y = cur_non_daily_user, col="Current non-daily user"), fun.y=mean, geom="point", shape=21, size=2) +
  xlab("Income Level") +
  ylab("Prevalence (percentage)") +
  ggtitle("Prevalence of smokers among U.S. adults, according to income level, 2017 BRFSS survey.") +
  scale_x_discrete(labels = c('<$10,000', '$10,000\n-\n$15,000', '$15,000\n-\n$20,000', '$20,000\n-\n$25,000', '$25,000\n-\n$35,000', '$35,000\n-\n$50,000', '$50,000\n-\n$75,000', '>=$75,000')) +
  scale_y_continuous(labels=percent_format()) +
  theme_minimal() +
  scale_color_manual("", values = c("Current daily user"="red","Current non-daily user"="blue"))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

rm(df_ecig, df_smoker)
########################################################################


# CASE 7 - Patterns across States.
library(usmap)
df_ecig = brfss_filtered
df_ecig = df_ecig[df_ecig$ecigsts == 1 | df_ecig$ecigsts == 2 | df_ecig$ecigsts == 3 |df_ecig$ecigsts == 4, ]
df_ecig = df_ecig[c('x.state', 'ecigsts')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_ecig$ecigsts == 1, 1, 0)
cur_non_daily_user = ifelse(df_ecig$ecigsts == 2, 1, 0)

# Attach to the ecig data frame.
df_ecig = cbind(df_ecig, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA values.
df_ecig = df_ecig[complete.cases(df_ecig[ , c('x.state')]), ]
# Filter out respondents who do have x.race not in {1, 2, ..., 56} (removing Guam and Puerto Rico)

df_ecig_aggregate_daily <- aggregate(cur_daily_user ~ x.state, data=df_ecig, mean)
names(df_ecig_aggregate_daily) <- c("fips", "cur_daily_user")
p1 <- plot_usmap(data = df_ecig_aggregate_daily, values = "cur_daily_user", lines = "black") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Current daily e-cigarette users", label = percent_format()) + 
  theme(legend.position = "right") + labs()

df_ecig_aggregate_non_daily <- aggregate(cur_non_daily_user ~ x.state, data=df_ecig, mean)
names(df_ecig_aggregate_non_daily) <- c("fips", "cur_non_daily_user")
p2 <- plot_usmap(data = df_ecig_aggregate_non_daily, values = "cur_non_daily_user", lines = "black") + 
  scale_fill_continuous(low = "white", high = "red", name = "Current non-daily e-cigarette users", label = percent_format()) + 
  theme(legend.position = "right") + labs()

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
rm(p1, p2)

# Copy data for processing smoker use.
df_smoker = brfss_filtered
df_smoker = df_smoker[df_smoker$smoker3 == 1 | df_smoker$smoker3 == 2 | df_smoker$smoker3 == 3 |df_smoker$smoker3 == 4, ]
df_smoker = df_smoker[c('x.state', 'smoker3')]

# Define current daily user and current user from the newly calculated variable.
cur_daily_user = ifelse(df_smoker$smoker3 == 1, 1, 0)
cur_non_daily_user = ifelse(df_smoker$smoker3 == 2, 1, 0)

# Attach to the ecig data frame.
df_smoker = cbind(df_smoker, cur_daily_user, cur_non_daily_user)

# Delete temp variales.
rm(cur_daily_user, cur_non_daily_user)

# Filter out NA values.
df_smoker = df_smoker[complete.cases(df_smoker[ , c('x.state')]), ]
# Filter out respondents who do have x.state not in {1, 2, ..., 56} (removing Guam and Puerto Rico)

df_smoker_aggregate_daily <- aggregate(cur_daily_user ~ x.state, data=df_smoker, mean)
names(df_smoker_aggregate_daily) <- c("fips", "cur_daily_user")
p1 <- plot_usmap(data = df_smoker_aggregate_daily, values = "cur_daily_user", lines = "black") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Current daily smokers", label = percent_format()) + 
  theme(legend.position = "right") + labs()

df_smoker_aggregate_non_daily <- aggregate(cur_non_daily_user ~ x.state, data=df_smoker, mean)
names(df_smoker_aggregate_non_daily) <- c("fips", "cur_non_daily_user")
p2 <- plot_usmap(data = df_smoker_aggregate_non_daily, values = "cur_non_daily_user", lines = "black") + 
  scale_fill_continuous(low = "white", high = "red", name = "Current non-daily smokers", label = percent_format()) + 
  theme(legend.position = "right") + labs()

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

rm(df_ecig, df_smoker)
########################################################################
