# CPSC 605 Data Mining & Data Analysis
# Final Project: Exploratory Data Analysis - Framingham Dataset
# Author: Isaiah Steinke
# Last Modified: October 21, 2020
# Written & Debugged in R v.4.0.2

# Import dataset
Fram <- read.csv("Fram_clean.csv", header = TRUE)

# Summary statistics
summary(Fram)
cor(Fram)

# Scatterplot matrix of continuous/numeric variables
pairs(Fram[, c(2, 5, 10:15)])

# Boxplots of continuous/numeric variables
boxplot(Fram[, c(2, 5, 10:15)])

# Histograms
par(mfrow=c(4, 4)) # set plot area to 4×4 array
hist(Fram$male, main = NULL, xlab = "male")
hist(Fram$age, main = NULL, xlab = "age")
hist(Fram$education, main = NULL, xlab = "education")
hist(Fram$currentSmoker, main = NULL, xlab = "currentSmoker")
hist(Fram$cigsPerDay, main = NULL, xlab = "cigsPerDay")
hist(Fram$BPMeds, main = NULL, xlab = "BPMeds")
hist(Fram$prevalentStroke, main = NULL, xlab = "prevalentStroke")
hist(Fram$prevalentHyp, main = NULL, xlab = "prevalentHyp")
hist(Fram$diabetes, main = NULL, xlab = "diabetes")
hist(Fram$totChol, main = NULL, xlab = "totChol")
hist(Fram$sysBP, main = NULL, xlab = "sysBP")
hist(Fram$diaBP, main = NULL, xlab = "diaBP")
hist(Fram$BMI, main = NULL, xlab = "BMI")
hist(Fram$heartRate, main = NULL, xlab = "heartRate")
hist(Fram$glucose, main = NULL, xlab = "glucose")
hist(Fram$TenYearCHD, main = NULL, xlab = "TenYearCHD")
par(mfrow=c(1, 1)) # reset plot area