# PACISE Conference Paper
# Creating Training and Test Sets at 70-30 Split
# Author: Isaiah Steinke
# Last Modified: February 7, 2021
# Written & Debugged in R v.4.0.3

# Import datasets
Cleve <- read.csv("Cleve.csv", header = TRUE)
Fram <- read.csv("Fram_clean.csv", header = TRUE)

# Cleve dataset
# -------------
set.seed(477) # set seed for reproducibility

# select 70% of the rows for the training set
train <- sample(1:nrow(Cleve), 718, replace = FALSE)
Cleve.train <- Cleve[train, ] # training set (718 rows)
Cleve.test <- Cleve[-train, ] # test set (307 rows)

# Export training & test sets
write.csv(Cleve.train, "Cleve_train.csv", row.names = FALSE)
write.csv(Cleve.test, "Cleve_test.csv", row.names = FALSE)

# Fram dataset
# -------------
set.seed(66696) # set seed for reproducibility

# select 80% of the rows for the training set
train <- sample(1:nrow(Fram), 2893, replace = FALSE)
Fram.train <- Fram[train, ] # training set (2893 rows)
Fram.test <- Fram[-train, ] # test set (1239 rows)

# Export training & test sets
write.csv(Fram.train, "Fram_train.csv", row.names = FALSE)
write.csv(Fram.test, "Fram_test.csv", row.names = FALSE)

# It was verified that the same proportions of classes for the response
# variables in the training and test sets for both Cleve and Fram are
# roughly the same: ~51% for Cleve and ~15% for Fram.