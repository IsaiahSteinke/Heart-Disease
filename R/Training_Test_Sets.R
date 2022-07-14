# CPSC 605 Data Mining & Data Analysis
# Final Project: Creating Training and Test Sets
# Author: Isaiah Steinke
# Last Modified: October 22, 2020
# Written & Debugged in R v.4.0.2

# Import datasets
Cleve <- read.csv("Cleve.csv", header = TRUE)
Fram <- read.csv("Fram_clean.csv", header = TRUE)

# Cleve dataset
# -------------
set.seed(477) # set seed for reproducibility

# select 80% of the rows for the training set
train <- sample(1:nrow(Cleve), 820, replace = FALSE)
Cleve.train <- Cleve[train, ] # training set (820 rows)
Cleve.test <- Cleve[-train, ] # test set (205 rows)

# Export training & test sets
write.csv(Cleve.train, "Cleve_train.csv", row.names = FALSE)
write.csv(Cleve.test, "Cleve_test.csv", row.names = FALSE)

# Fram dataset
# -------------
set.seed(66696) # set seed for reproducibility

# select 80% of the rows for the training set
train <- sample(1:nrow(Fram), 3306, replace = FALSE)
Fram.train <- Fram[train, ] # training set (3306 rows)
Fram.test <- Fram[-train, ] # test set (826 rows)

# Export training & test sets
write.csv(Fram.train, "Fram_train.csv", row.names = FALSE)
write.csv(Fram.test, "Fram_test.csv", row.names = FALSE)

# Exploration of the response variable (TenYearCHD) in Weka shows that
# both the training and test sets have roughly the same proportions of zeroes
# and ones (the ones being the minority class at ~15% of the total data).
# That is, the proportion of ones is ~15% for TenYearCHD in both the training
# and test sets.