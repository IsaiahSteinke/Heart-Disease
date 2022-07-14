# CPSC 605 Data Mining & Data Analysis
# Final Project: Imputation Models for Missing Values for Categorical Variables
# Author: Isaiah Steinke
# Last Modified: October 13, 2020
# Written & Debugged in R v.4.0.2

# Load libraries
library(class) # v.7.3-17
library(tree) # v.1.0-40

# Import dataset
Fram <- read.csv("Fram.csv", header = TRUE)

# Find NA for heartRate variable; delete row
which(is.na(Fram$heartRate)) # row 690
Fram <- Fram[-690, ]

# ******************
# education variable
# ******************

# Prepare data
# ------------
# Delete other columns containing NA/missing values and TenYearCHD
# These columns are cigsPerDay, BPmeds, totChol, BMI, glucose
educ <- Fram[,-c(5, 6, 10, 13, 15, 16)]

# Create subset that has no rows with NAs for education
educ1 <- educ[!is.na(educ$education), ]

# Select rows for training set (just under 85%)
set.seed(1604) # set seed for reproducibility
train <- sample(1:nrow(educ1), 3512, replace = FALSE)

# Put true values of education for the test set into a vector
test.true <- educ1$education[-train]

# Create subset that only has NAs for education
educ2 <- educ[is.na(educ$education), ] # 105 missing values

# k nearest neighbors (kNN) model
# -------------------------------
# Can only use numeric data as predictors. This means that we need the dataset
# to only contain age, sysBP, diaBP, and heartRate. Scale numeric predictors
# so that they have means of zero and standard deviations of one since the 
# nearest neighbors are determined by the Euclidean distance.
educ1.std <- scale(educ1[, c(2, 8:10)])

# The following lines were repeatedly executed for different values of k
# to determine the best value.
set.seed(458915)
preds.knn <- knn(educ1.std[train, ], educ1.std[-train, ],
                 educ1$education[train], k = 7)
table(preds.knn, test.true) # confusion matrix

# k = 7; correct classification rate: ~39.2%
#          test.true
# preds.knn   1   2   3   4
#         1 161  98  50  36
#         2  67  63  39  19
#         3  17  15  13   5
#         4  14  10   7   6
# k = 25; correct classification rate: ~46.1%
#          test.true
# preds.knn   1   2   3   4
#         1 197  92  63  45
#         2  60  87  42  21
#         3   2   5   2   0
#         4   0   2   2   0
# As k increases, kNN tends to more aggressively classify into either class
# 1 or 2, as those make up ~70% of the data. k = 7 appears to have the best
# tradeoff, with a correct classification rate just under 40%. The best
# correct classification rate was obtained at k = 25 for the values I tested.

# Decision tree model
# -------------------
tree.educ <- tree(as.factor(education) ~ ., data = educ1, subset = train)
summary(tree.educ)
plot(tree.educ)
text(tree.educ, pretty = 0)
# Only builds a tree with one split using age. Classifies as either 1 or 2.
# Misclassification error rate: 0.539 = 1893 / 3512.
# Similar performance as kNN on the training set (46.1%).
preds.tree <- predict(tree.educ, newdata = educ1[-train, ], type = "class")
table(preds.tree, test.true)
#           test.true
# preds.tree   1   2   3   4
#          1 200  95  74  44
#          2  59  91  35  22
#          3   0   0   0   0
#          4   0   0   0   0
# Correct classification rate on test set: ~47%.
# Slightly better than kNN but doesn't predict classes 3 or 4! :-\

# ***************
# BPMeds variable
# ***************

# Prepare data
# ------------
# Delete other columns containing NA/missing values and TenYearCHD
# These columns are education, cigsPerDay, totChol, BMI, glucose
BPM <- Fram[,-c(3, 5, 10, 13, 15, 16)]

# Create subset that has no rows with NAs for BPMeds
BPM1 <- BPM[!is.na(BPM$BPMeds), ]

# Select rows for training set (just under 85%)
set.seed(6715069) # set seed for reproducibility
train <- sample(1:nrow(BPM1), 3556, replace = FALSE)

# Put true values of education for the test set into a vector
test.true <- BPM1$BPMeds[-train]

# Create subset that only has NAs for education
BPM2 <- BPM[is.na(BPM$BPMeds), ] # 53 missing values

# k nearest neighbors model
# -------------------------
# Again, only use numeric data as predictors and scale them.
BPM1.std <- scale(BPM1[, c(2, 8:10)])

# The following lines were repeatedly executed for different values of k
# to determine the best value. k = 1 or 3 appears to be the best values.
set.seed(1832)
preds.knn <- knn(BPM1.std[train, ], BPM1.std[-train, ],
                 BPM1$BPMeds[train], k = 11)
table(preds.knn, test.true) # confusion matrix

# k = 11; correct classification rate: ~98.1%
#          test.true
# preds.knn   0   1
#         0 616  12
#         1   0   0

# Out of 4184 rows in BPM1, only 124 have BPMeds as 1. The kNN results
# seem to suggest that just imputing 0 for the missing data would be a viable
# solution. There isn't any value of k for which a 1 is predicted and
# correctly classified.

# Decision tree model
# -------------------
tree.BPM <- tree(as.factor(BPMeds) ~ ., data = BPM1, subset = train)
summary(tree.BPM)
plot(tree.BPM)
text(tree.BPM, pretty = 0)
# Builds a tree with four variables. All classes are 0 (i.e., no nodes are 1).
# Misclassification error rate: 0.0315 = 112 / 3556.
preds.tree <- predict(tree.BPM, newdata = BPM1[-train, ], type = "class")
table(preds.tree, test.true)
#           test.true
# preds.tree   0   1
#          0 616  12
#          1   0   0
# Same as kNN.