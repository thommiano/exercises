
################################################################################
################################################################################
# Author: Thom Miano
# Project: RTI Exercise 01
# Purpose: Simple analysis on the data
################################################################################
################################################################################

# Set working directory
setwd("~/GitHub/exercises/exercise01/src")

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

################################################################################
### Build model



glm1 <- glm(over_50k ~ ., family = "binomial", data = train)
summary(glm1)

library(RWeka)
library(rpart)

tree <- rpart(over_50k ~ ., data = train)
tree.validation.prediction <- predict(tree, validate[, -15])

validation.accuracy <- sum(validate[15] == tree.validation.prediction) / length(tree.validation.prediction)

### Random forest
library(randomForest)

set.seed(757)
fit <- randomForest(as.factor(over_50k) ~ ., data = keep, importance=TRUE, ntree=200)

varImpPlot(fit)

### Condition forest trees
library(party)

set.seed(757)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

################################################################################
### Evaluation

################################################################################
################################################################################
### References