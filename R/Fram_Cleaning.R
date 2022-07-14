# CPSC 605 Data Mining & Data Analysis
# Final Project: Cleaning the Framingham Dataset
# Author: Isaiah Steinke
# Last Modified: October 21, 2020
# Written & Debugged in R v.4.0.2

# Import dataset
Fram <- read.csv("Fram.csv", header = TRUE)

# heartRate: Find NA and delete row
# Only one row, so should be able to delete without much loss of data
which(is.na(Fram$heartRate)) # row 690
Fram <- Fram[-690, ]

# education: Find NAs and delete rows
# Tried to fit kNN and decision tree models to impute missing values (NAs),
# but this did not provide good results (<50% correct values). Thus, delete
# these rows since the data are heavily biased to classes 1 and 2 (~70%).
educ.del <- which(is.na(Fram$education))
Fram <- Fram[-educ.del, ]

# BPMeds: Impute 0.
# Tried to fit kNN and decision tree models. Good classification results are
# achieved (~98% correct values on test set), but this is because the data are
# mostly class 0. We can achieve similar correctness by just imputing 0 for the
# NAs. A logistic model was not good (poor R-squared value).
Fram$BPMeds[is.na(Fram$BPMeds)] <- 0

# cigsPerDay: Impute value of 0.
# Tried to fit a regression model but did not achieve a good R-squared
# value. Histogram is quite skewed with about half of the values being 0
# or 1. Since there's only 29 missing values/NAs, we'll just impute 0.
Fram$cigsPerDay[is.na(Fram$cigsPerDay)] <- 0

# For the following variables originally containing NAs, we decided to impute
# the median values of these variables. We attempted to fit regression models
# to impute missing values better, but these models did not have very good
# values of R-squared (less than 0.3 at best). This is most likely because
# there were only a few predictors available for prediction after removing
# all other variables with NAs and the response variable (TenYearCHD). Of
# these remaining variables, many of them were categorical with two levels.
# Both the median and mean had similar values for these variables.

# totChol: Impute median value of 234.
Fram$totChol[is.na(Fram$totChol)] <- 234

# BMI: Impute median value of 25.39.
Fram$BMI[is.na(Fram$BMI)] <- 25.39

# glucose: Impute median value of 78.
Fram$glucose[is.na(Fram$glucose)] <- 78

# Export cleaned dataset
write.csv(Fram, "Fram_clean.csv", row.names = FALSE)
