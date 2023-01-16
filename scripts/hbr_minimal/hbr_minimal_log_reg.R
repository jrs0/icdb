##' Perform a logistic regression to attempt to predict bleeding outcome from the
##' minimal HBR dataset
##'
##' 

library(tidyverse)
library(caret)
library(corrplot)
library(pROC)

hbr_minimal_dataset_test <- readRDS("gendata/hbr_minimal_dataset_test.rds")
hbr_minimal_dataset_train <- readRDS("gendata/hbr_minimal_dataset_train.rds")

## Get the predictors for the test and train sets
predictors_test <- hbr_minimal_dataset_test %>%
    select(-date, -bleed)
predictors_train <- hbr_minimal_dataset_train %>%
    select(-date, -bleed)

## Get the response from the test and train sets
response_test <- hbr_minimal_dataset_test %>%
    select(bleed)
response_train <- hbr_minimal_dataset_train %>%
    select(bleed)

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM). 

## Remove zero-variance predictors
near_zero_var_indices <- nearZeroVar(predictors_train)
predictors_train <- predictors_train %>%
    select(-near_zero_var_indices)

## TODO Deal with class inbalance here


set.seed(476)

## Reverse the order of the factors to fit with the
## twoClassSummary below
dataset$bleed <- factor(dataset$bleed,
                        levels=rev(levels(dataset$bleed)))

## Use 10-fold cross validation
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
lr_full <- train(dataset[,1:2],
                 y = dataset$bleed,
                 method = "glm",
                 metric = "ROC",
                 trControl = ctrl)

## View the summary, look for ROC area, which is the average
## over the n folds of the cross validation
lr_full

## Predict the class probabilities and add the probability of
lr_full_pred_prob <- predict(lr_full, new_data = dataset, type = "prob")
dataset$lr_bleed_prob <- lr_full_pred_prob[,"bleed_occured"]

## Plot the ROC curve from the predicted probabilities
roc_curve <- roc(response = dataset$bleed,
                predictor = dataset$lr_bleed_prob)

## Show the area under the ROC curve with a confidence interval
auc(roc_curve)
ci(roc_curve)

## Plot the ROC curve
plot(roc_curve, legacy.axes = TRUE)

## Predict the class from the LR model
dataset$lr_bleed <- predict(lr_full, new_data = dataset)

## Plot the confusion matrix 
confusionMatrix(prediction, dataset$bleed)

