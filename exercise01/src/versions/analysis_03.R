
################################################################################
################################################################################
# Author: Thom Miano
# Project: RTI Exercise 01
# Purpose: Simple analysis on the data
################################################################################
################################################################################

# Set working directory
setwd("~/GitHub/exercises/exercise01/src")
library(beepr) # For knowning when computations complete

# Load data
records <- read.csv("../data/exercise01_flat.csv", header = TRUE)

################################################################################
### Exploratory data analysis

# Generate histograms (density for numeric) and barplots (for categorical)
for(i in 1:ncol(records)) {
  tryCatch(hist(records[[i]], main = deparse(colnames(records[i]))), 
           error=function(e) {
    tryCatch(barplot(table(records[[i]]),  main = deparse(colnames(records[i]))), 
             error=function(e) {
      print('Error')
    })
  })  
}


# Generate summary statistics
summary(records)
## Comments:
# Missing 2799 observations for workclass_id
# Missing 857 observations for country_id

# We can quickly see where and how many missing values we have (marked as "?")
for (i in 1:nrow(records)) {
  print(paste0(colnames(records[i]), ": ",
               nrow(records[records[, i] == "?", ])))
}

# Now we also see that occupation_id is missing 2809 observations

# Let's check to see if we have any other types of missing values
# NA
for (i in 1:nrow(records)) {
  print(paste0(colnames(records[i]), ": ",
               nrow(records[is.na(records[, i]), ])))
}
# NaN
for (i in 1:nrow(records)) {
  print(paste0(colnames(records[i]), ": ",
               nrow(records[is.nan(records[, i]), ])))
}
# "" i.e., empty
for (i in 1:nrow(records)) {
  print(paste0(colnames(records[i]), ": ",
               nrow(records[records[, i] == "", ])))
}
# " " i.e., a space
for (i in 1:nrow(records)) {
  print(paste0(colnames(records[i]), ": ",
               nrow(records[records[, i] == " ", ])))
}

# Ok, so it looks like we're set on which observations are missing.
# We could deal with the missing values several different ways. One way could
# be to throw out any rows with missing data. Another more complicated way 
# could be to try and estimate the values of the missing information based
# the the information that we do have for those rows that have missing data.

# There are multiple paths to removing the missing values. A quick way is to
# turn the ? into NAs, and then we can use na.omit()
records[records == "?"] <- NA
keep <- na.omit(records)

## Note: "Undefined columns selected" errors will be generated, but the results
## we desire will also be generated without any problem.

# Making sure we don't have any NAs
for (i in 1:nrow(keep)) {
  print(paste0(colnames(keep[i]), ": ",
               nrow(records[is.na(keep[, i]), ])))
}

# And making sure we don't have any "?"
for (i in 1:nrow(keep)) {
  print(paste0(colnames(keep[i]), ": ",
               nrow(records[keep[, i] == "?", ])))
}

# We have eliminated all of our rows with missing data.
# Since our other values came from normalized tables, it doesn't look like
# there's any other preprocessing we need to do.

#write.csv(keep, "../data/exercise01_keep.csv", row.names = FALSE)

################################################################################
### Break data into training, validation, and test data sets

# Load methods for sampling data and evaluating metrics
source("evaluation_methods.R")

# Create our training, validation, and test
random.sample(keep)
# By default this method is set to make training data 80% of orginial data
# The method defines the other 20% as "validate" so that it can manually be
# assigned as "test" and so that the method can be used again on our training
# data without needing to rename another set of data.
test <- validate

# We will run the random.sample on the train data to create a validate set
random.sample(train)

## Note: Originally I wrote out my samples so my results were completely
## reproducible. Somewhere there is a problem with loading this data and then
## using it for building the models. So I took this part out so that the code
## otherwise runs without any problems.

# Export the data I'm using for my following analysis:
# write.csv(train, "../data/my_train.csv", row.names = FALSE)
# write.csv(validate, "../data/my_validate.csv", row.names = FALSE)
# write.csv(test, "../data/my_test.csv", row.names = FALSE)

################################################################################
### Build model

# This will load the data I was using to get the results I generated below:
# train <- read.csv("../data/my_train.csv", header = TRUE)
# validate <- read.csv("../data/my_validate.csv", header = TRUE)
# test <- read.csv("../data/my_test.csv", header = TRUE)

### Using Random forest
library(randomForest)

########################################
### Evaluation

### Run 1
model <- "random forest"
set.seed(756) # For reproducibility
seed <- 756

# Create the model with the training data
fit <- randomForest(as.factor(over_50k) ~ ., data = train, importance=TRUE, ntree=100)
ntree <- 100 # I'm reducing the number of trees just to a get a sense of how much the 
# increased count is actually helping us, if infact it is.

# Predict with validation
predict.time <- proc.time()[[3]]
prediction <- predict(fit, validate, OOB = TRUE, type = "response")
model.time <- proc.time()[[3]] - predict.time # This records the time it took to test

# Calculate sensitivity, specificity, accuracy
metric.eval(validate$over_50k, prediction) 

# Add values to global table
rforest.evaluation <- data.frame(cbind(model, sensitivity, specificity, 
                                       accuracy, ntree, model.time, seed))

### Run 2
model <- "random forest"
set.seed(757) # For reproducibility
seed <- 757

# Create the model with the training data
fit <- randomForest(as.factor(over_50k) ~ ., data = train, importance=TRUE, ntree=200)
ntree <- 200

# Predict with validation
predict.time <- proc.time()[[3]]
prediction <- predict(fit, validate, OOB = TRUE, type = "response")
model.time <- proc.time()[[3]] - predict.time # This records the time it took to test

# Calculate sensitivity, specificity, accuracy
metric.eval(validate$over_50k, prediction) 

# Add values to global table
rforest.vector <- data.frame(cbind(model, sensitivity, specificity, 
                                   accuracy, ntree, model.time, seed))
rforest.evaluation <- rbind(rforest.evaluation, rforest.vector)

### Run 3
model <- "random forest"
set.seed(758) # For reproducibility
seed <- 758

# Create the model with the training data
fit <- randomForest(as.factor(over_50k) ~ ., data = train, importance=TRUE, ntree=300)
ntree <- 300

# Predict with validation
predict.time <- proc.time()[[3]]
prediction <- predict(fit, validate, OOB = TRUE, type = "response")
model.time <- proc.time()[[3]] - predict.time # This records the time it took to test

# Calculate sensitivity, specificity, accuracy
metric.eval(validate$over_50k, prediction) 

# Add values to global table
rforest.vector <- data.frame(cbind(model, sensitivity, specificity, 
                                   accuracy, ntree, model.time, seed))
rforest.evaluation <- rbind(rforest.evaluation, rforest.vector)

### Run 4
model <- "random forest"
set.seed(759) # For reproducibility
seed <- 759

# Create the model with the training data
fit <- randomForest(as.factor(over_50k) ~ ., data = train, importance=TRUE, ntree=400)
ntree <- 400

# Predict with validation
predict.time <- proc.time()[[3]]
prediction <- predict(fit, validate, OOB = TRUE, type = "response")
model.time <- proc.time()[[3]] - predict.time # This records the time it took to test

# Calculate sensitivity, specificity, accuracy
metric.eval(validate$over_50k, prediction) 

# Add values to global table
rforest.vector <- data.frame(cbind(model, sensitivity, specificity, 
                                   accuracy, ntree, model.time, seed))
rforest.evaluation <- rbind(rforest.evaluation, rforest.vector)
beep(2)

#write.csv(rforest.evaluation, "../data/rforest_evaluation.csv", row.names = FALSE)

########################################
### Testing

# Read in the evaluation data
rforest.evaluation.load <- read.csv("../data/rforest_evaluation.csv", header = TRUE)
# Looking at our data we see that overall each model performs pretty consistently,
# despite the increase in number of trees. Ideally we would have considered a variety
# of other parameters, but for the purposes of this assignment we will leave it there.
# Given the noticably faster computation time, we will use 100 trees for our testing.

### Run 1
model <- "random forest"
set.seed(600) # For reproducibility
seed <- 600

# Create the model with the training data
fit <- randomForest(as.factor(over_50k) ~ ., data = train, importance=TRUE, ntree=100)
ntree <- 100 # I'm reducing the number of trees just to a get a sense of how much the 
# increased count is actually helping us, if infact it is.

# Predict with validation
predict.time <- proc.time()[[3]]
prediction <- predict(fit, test, OOB = TRUE, type = "response")
model.time <- proc.time()[[3]] - predict.time # This records the time it took to test

# Calculate sensitivity, specificity, accuracy
metric.eval(test$over_50k, prediction) 

# Add values to global table
rforest.test <- data.frame(cbind(model, sensitivity, specificity, 
                                       accuracy, ntree, model.time, seed))

### Run 2
model <- "random forest"
set.seed(601) # For reproducibility
seed <- 601

# Create the model with the training data
fit <- randomForest(as.factor(over_50k) ~ ., data = train, importance=TRUE, ntree=100)
ntree <- 100

# Predict with validation
predict.time <- proc.time()[[3]]
prediction <- predict(fit, test, OOB = TRUE, type = "response")
model.time <- proc.time()[[3]] - predict.time # This records the time it took to test

# Calculate sensitivity, specificity, accuracy
metric.eval(test$over_50k, prediction) 

# Add values to global table
rforest.vector <- data.frame(cbind(model, sensitivity, specificity, 
                                   accuracy, ntree, model.time, seed))
rforest.test <- rbind(rforest.test, rforest.vector)

### Run 3
model <- "random forest"
set.seed(602) # For reproducibility
seed <- 602

# Create the model with the training data
fit <- randomForest(as.factor(over_50k) ~ ., data = train, importance=TRUE, ntree=100)
ntree <- 100

# Predict with validation
predict.time <- proc.time()[[3]]
prediction <- predict(fit, test, OOB = TRUE, type = "response")
model.time <- proc.time()[[3]] - predict.time # This records the time it took to test

# Calculate sensitivity, specificity, accuracy
metric.eval(test$over_50k, prediction) 

# Add values to global table
rforest.vector <- data.frame(cbind(model, sensitivity, specificity, 
                                   accuracy, ntree, model.time, seed))
rforest.test <- rbind(rforest.test, rforest.vector)

### Run 4
model <- "random forest"
set.seed(603) # For reproducibility
seed <- 603

# Create the model with the training data
fit <- randomForest(as.factor(over_50k) ~ ., data = train, importance=TRUE, ntree=100)
ntree <- 100

# Predict with validation
predict.time <- proc.time()[[3]]
prediction <- predict(fit, test, OOB = TRUE, type = "response")
model.time <- proc.time()[[3]] - predict.time # This records the time it took to test

# Calculate sensitivity, specificity, accuracy
metric.eval(test$over_50k, prediction) 

# Add values to global table
rforest.vector <- data.frame(cbind(model, sensitivity, specificity, 
                                   accuracy, ntree, model.time, seed))
rforest.test <- rbind(rforest.test, rforest.vector)
beep(2)

#write.csv(rforest.test, "../data/rforest_test.csv", row.names = FALSE)

########################################
### Conclusion

# Load the original results
rforest.test.load <- read.csv("../data/rforest_test.csv", header = TRUE)
# Interestingly we see that for the four runs, all of the metrics turned out the same.
# I suspect this means that in each run, the random forest model decided on the exact
# same solution each time.

# Let's generate a plot to see which of our variables ended up being important:
varImpPlot(fit, main = "Random Forest Variable Importance")

# png("../images/rforest_variable_importance.png")
# varImpPlot(fit, main = "Random Forest Variable Importance")
# dev.off()

# Typically I would use 10-fold cross-validation, using stratified samples for 
# our data sets. But I believe this is sufficient for the purposes of this assignment.
# Additionally, given more time I would have considered other classification
# methods like kNN and conditional inferences trees, and I would have compared
# the performance.

################################################################################
################################################################################
### References