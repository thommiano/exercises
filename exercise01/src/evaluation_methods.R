
################################################################################
################################################################################
# Author: Thom Miano
# Project: RTI Exercise 01
# Purpose: Methods for sampling data and metric evaluations
# Date: 09.02.15
################################################################################
################################################################################

################################################################################
### Random Sample
## This randomly samples from the input data set, selecting r% of the data
## to training and 1-r% of the data to testing

random.sample <- function (data, r=.8, option = 2) {
  
  # Number of rows in the data set
  n.points <- nrow(data)
  
  # User defined sampling rate, set to 80% as default
  sampling.rate <- r
  
  # Randomly sample which rows will go in the training set
  training <- sample(1:n.points, sampling.rate * n.points, replace = FALSE)
  # Define the training set to be those rows
  .GlobalEnv$train <- subset(data[training, ])
  
  # The other rows are going into the test set
  testing <- setdiff(1:n.points, training)
  # Define the test set to be the other rows
  .GlobalEnv$validate <- subset(data[testing, ]) # Figure out how to have this take "
}


################################################################################
### Metric Evaluation
## This calculates the true positives, true negatives, false positives, and 
## false negatives for data G -- ground truth -- and P -- prediction.

metric.eval <- function (G, P) {
  TP <- 0
  TN <- 0
  FP <- 0
  FN <- 0
  for (i in 1:length(G)) {
    if (G[i] == 1) { # Ground truth: positive
      if (G[i] == P[i]) { # When the prediction matches
        TP <- TP + 1 # Observe a true positive
      } else {
        FN <- FN + 1 # Observe a false negative
      }
    } else { # Ground truth: negative
      if (G[i] == P[i]) { # When the prediction matches
        TN <- TN + 1 # Observe a true negative
      } else {
        FP <- FP + 1 # Observe a false positive
      }
    }
  }
  
#   .GlobalEnv$TP <- TP
#   .GlobalEnv$TN <- TN
#   .GlobalEnv$FP <- FP
#   .GlobalEnv$FN <- FN
  
  # Sensitivity
  .GlobalEnv$sensitivity <- (TP / (TP + FN))
  # Specificity
  .GlobalEnv$specificity <- (TN / (TN + FP))
  # testofaccuracy
  .GlobalEnv$accuracy <- ((TP + TN) / (TP + TN + FP + FN))
  # error rate
#   .GlobalEnv$error.rate <- 1 - .GlobalEnv$test.accuracy
  
  # Add the final metric calculations
}
################################################################################
################################################################################
### References

# random.sample method taken from the following:
# O'Reilly, "Doing Data Science", pp. 77-78
