# CPSC 605 Data Mining & Data Analysis
# Final Project: Analysis of Datasets with Tree-Based Methods
# Author: Isaiah Steinke
# Last Modified: November 3, 2020
# Written & Debugged in R v.4.0.2

# Load required libraries
library(tree) # v.1.0-40
library(randomForest) # v.4.6-14
library(gbm) # v.2.1.8

# ==========================
# Expanded Cleveland Dataset
# ==========================
# Import dataset and prepare it for analysis
Cleve.train <- read.csv("Cleve_train.csv", header = TRUE) # training set
cols <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca",
          "thal", "target") # columns with categorical variables
Cleve.train[, cols] <- lapply(Cleve.train[, cols], as.factor) # set as factors
str(Cleve.train)
Cleve.test <- read.csv("Cleve_test.csv", header = TRUE) # test set
Cleve.test[, cols] <- lapply(Cleve.test[, cols], as.factor)
str(Cleve.test)
Cleve.truth <- Cleve.test$target # vector of true response values
Cleve.test <- Cleve.test[, -14] # remove response variable

# Decision tree
start.time <- proc.time() # start timer
Cleve.dt <- tree(target ~ ., data = Cleve.train, split = "gini")
summary(Cleve.dt)
dt.preds <- predict(Cleve.dt, newdata = Cleve.test, type = "class")
table(dt.preds, Cleve.truth)
proc.time() - start.time # calculate runtime
plot(Cleve.dt) # plot tree
text(Cleve.dt, pretty = 0)

# Random Forests/Bagging
# The following lines were repeatedly executed for various values of the
# hyperparameters mtry and ntree. The results were tabulated in an Excel
# spreadsheet. The runtimes appear to vary by +/-0.01 s for repeated
# executions of the same code.
# ******
start.time <- proc.time() # start timer
set.seed(85) # set seed for reproducibility
Cleve.rf <- randomForest(target ~ ., data = Cleve.train, mtry = 4, ntree = 100,
                         importance = TRUE)
rf.preds <- predict(Cleve.rf, newdata = Cleve.test, type = "class")
table(rf.preds, Cleve.truth)
proc.time() - start.time # calculate runtime
# ******
# These lines are used to produce plots showing the most important variables.
varImpPlot(Cleve.rf, type = 1)
varImpPlot(Cleve.rf, type = 2)

# Boosting
# Need to set response variable to binary to use boosting in gbm
Cleve.train <- read.csv("Cleve_train.csv", header = TRUE) # training set
cols <- c("sex", "cp", "fbs", "restecg", "exang", "slope", "ca",
          "thal") # columns with categorical variables
Cleve.train[, cols] <- lapply(Cleve.train[, cols], as.factor) # set as factors
# The following lines were repeatedly executed for various values of the
# hyperparameters n.trees, interaction.depth, and shrinkage. The results were
# tabulated in an Excel spreadsheet. The runtimes appear to vary by +/-0.01 s
# for repeated executions of the same code.
# ******
start.time <- proc.time() # start timer
set.seed(85)
Cleve.boost <- gbm(target ~ ., data = Cleve.train, distribution = "bernoulli",
                   n.trees = 500, interaction.depth = 2, shrinkage = 0.2)
boost.probs <- predict(Cleve.boost, newdata = Cleve.test, n.trees = 500,
                       type = "response")
boost.preds <- rep(0, 205)
boost.preds[boost.probs > 0.5] <- 1
table(boost.preds, Cleve.truth)
proc.time() - start.time # calculate runtime
# ******
summary(Cleve.boost) # relative influence plot/statistics

# ==================
# Framingham Dataset
# ==================
# Import dataset and prepare it for analysis
Fram.train <- read.csv("Fram_train.csv", header = TRUE) # training set
cols <- c("male", "education", "currentSmoker", "BPMeds", "prevalentStroke",
          "prevalentHyp", "diabetes", "TenYearCHD") # categorical variables
Fram.train[, cols] <- lapply(Fram.train[, cols], as.factor) # set as factors
str(Fram.train)
Fram.test <- read.csv("Fram_test.csv", header = TRUE) # test set
Fram.test[, cols] <- lapply(Fram.test[, cols], as.factor)
str(Fram.test)
Fram.truth <- Fram.test$TenYearCHD # vector of the true values of the response
Fram.test <- Fram.test[, -16] # remove response variable

# Decision tree
start.time <- proc.time() # start timer
set.seed(85)
Fram.dt <- tree(TenYearCHD ~ ., data = Fram.train, split = "gini")
summary(Fram.dt)
dt.preds <- predict(Fram.dt, newdata = Fram.test, type = "class")
table(dt.preds, Fram.truth)
proc.time() - start.time # calculate runtime
plot(Fram.dt) # plot tree
text(Fram.dt, pretty = 0)

# Random Forests/Bagging
# The following lines were repeatedly executed for various values of the
# hyperparameters mtry and ntree. The results were tabulated in an Excel
# spreadsheet. The runtimes appear to vary by +/-0.06 s for repeated
# executions of the same code.
# ******
start.time <- proc.time() # start timer
set.seed(85) # set seed for reproducibility
Fram.rf <- randomForest(TenYearCHD ~ ., data = Fram.train, mtry = 15,
                        ntree = 200, importance = TRUE)
rf.preds <- predict(Fram.rf, newdata = Fram.test, type = "class")
table(rf.preds, Fram.truth)
proc.time() - start.time # calculate runtime
# ******
# These lines are used to produce plots showing the most important variables.
varImpPlot(Fram.rf, type = 1)
varImpPlot(Fram.rf, type = 2)

# Boosting
# Need to set response variable to binary to use boosting in gbm
Fram.train <- read.csv("Fram_train.csv", header = TRUE) # training set
cols <- c("male", "education", "currentSmoker", "BPMeds", "prevalentStroke",
          "prevalentHyp", "diabetes") # categorical variables
Fram.train[, cols] <- lapply(Fram.train[, cols], as.factor) # set as factors
# The following lines were repeatedly executed for various values of the
# hyperparameters n.trees, interaction.depth, and shrinkage. The results were
# tabulated in an Excel spreadsheet. The runtimes appear to vary by +/-0.02 s
# for repeated executions of the same code.
# ******
start.time <- proc.time() # start timer
set.seed(85)
Fram.boost <- gbm(TenYearCHD ~ ., data = Fram.train, distribution = "bernoulli",
                  n.trees = 2500, interaction.depth = 5, shrinkage = 0.1)
boost.probs <- predict(Fram.boost, newdata = Fram.test, n.trees = 2500,
                       type = "response")
boost.preds <- rep(0, 826)
boost.preds[boost.probs > 0.5] <- 1
table(boost.preds, Fram.truth)
proc.time() - start.time # calculate runtime
# ******
summary(Fram.boost) # relative influence plot/statistics