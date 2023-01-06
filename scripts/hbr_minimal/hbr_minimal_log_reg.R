##' Perform a logistic regression to attempt to predict bleeding outcome from the
##' minimal HBR dataset
##'
##' 

library(tidyverse)
library(caret)
library(corrplot)

hbr_minimal_dataset <- readRDS("gendata/hbr_minimal_dataset.rds")

## Drop the date from the predictors
without_date <- hbr_minimal_dataset %>%
    select(-date)

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

## Do the logistic regression
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)
set.seed(476)
lr_full <- train(dataset,
                 y = dataset$bleed,
                 method = "glm",
                 metric = "ROC",
                 trControl = ctrl)





## Stuff
model_fit <- glm(bleed ~ .,
                 ## Select the rows for the pre-2008 data:
                 data = dataset,
                 ## 'family' relates to the distribution of the data.
                 ## A value of 'binomial' is used for logistic regression
                 family = binomial)
