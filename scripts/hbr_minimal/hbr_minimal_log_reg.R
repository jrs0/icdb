##' Perform a logistic regression to attempt to predict bleeding outcome from the
##' minimal HBR dataset
##'
##' 

library(tidyverse)
library(caret)
library(corrplot)
library(pROC)

hbr_minimal_dataset <- readRDS("gendata/hbr_minimal_dataset.rds")

## Drop the date from the predictors
without_date <- hbr_minimal_dataset %>%
    select(-date)

model_fit <- glm(bleed ~ .,
                 ## Select the rows for the pre-2008 data:
                 data = without_date,
                 ## 'family' relates to the distribution of the data.
                 ## A value of 'binomial' is used for logistic regression
                 family = binomial)


predictors <- without_date %>%
    select(-bleed)

## Remove zero variance predictors
near_zero_indices <- nearZeroVar(predictors)
predictors <- predictors %>%
    select(-near_zero_indices)

## Show the remaining columns
predictors %>% colnames()

## ## Encode the remaining factor columns as dummy variables
## dummy_mod <- dummyVars(~ ., data = predictors)
## dummy_predictors <- predict(dummy_mod, predictors)
## dummy_predictors <- as.data.frame(dummy_predictors)
## dummy_predictors <- as_tibble(dummy_predictors)

## dataset$age <- without_date$age
## dataset$bleed <- without_date$bleed

## View correlations in the predictors
## cor <- cor(predictors)
## corrplot(cor)

## Remove zero variance

## Make the dataset
dataset <- predictors
dataset$bleed <- without_date$bleed

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
set.seed(476)
lr_full <- train(dataset[,1:2],
                 y = dataset$bleed,
                 method = "glm",
                 metric = "ROC",
                 trControl = ctrl)


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



## Basic LR

glm_model <- glm(bleed ~ .,
                 ## Select the rows for the pre-2008 data:
                 data = dataset,
                 ## 'family' relates to the distribution of the data.
                 ## A value of 'binomial' is used for logistic regression
                 family = binomial)
