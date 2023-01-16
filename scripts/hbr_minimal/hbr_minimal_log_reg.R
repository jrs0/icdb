##' Perform a logistic regression to attempt to predict bleeding outcome from the
##' minimal HBR dataset
##'
##' 

library(caret)
library(corrplot)
library(pROC)
library(tidyverse)

##' @title Get ROC curves for the cross-validation folds
##' @return A tibble containing columns for the sensitivities,
##' specificities, and cross-validation fold in the label column
get_cv_roc <- function(fit)
{
    ## From "https://stackoverflow.com/questions/69246553/
    ## plot-the-average-cross-validated-auc-from-caret-package"
    ##
    ## sapply roc(), bind as tibble with Resample as .id
    tbl <- sapply(X = unique(fit$pred$Resample),
           FUN = function(x) {
               r <- fit$pred[fit$pred$Resample == x,]
               R <- roc(response = r$obs, predictor = r$bleed_occured)
               data.frame(sensitivities = R$sensitivities,
                          specificities = R$specificities)
           }, simplify = F) %>%
        bind_rows(.id = "Resample") %>%
        as_tibble() %>%
        rename(label = Resample)
}

##' @title Add predictions and class probabilities to the data
##' @param data A tibble containing the predictors used in fit
##' and the response column 
##' @param fit The model to use
##' @param response_name The string name of the column to use as
##' the response (a factor)
##' @param positive_event Which level of response to use as the
##' positive event for the purpose of probabilities
##' @param response The response column to predict. The argument is
##' a string. 
##' @return A tibble containing the new columns response_pred for
##' predictions, and response_prob for the prediction probabilities
##' 
add_predictions <- function(data, fit, response, positive_event)
{

    ## What is going on here? Apparently putting paste0 into the mutate
    ## does not work
    response_pred <- paste0(response, "_pred") 
    data[[response_pred]] = predict(fit, newdata = data)

    response_prob <- paste0(response, "_prob") 
    data[[response_prob]] = predict(fit, newdata = data,
                                    type = "prob")[,positive_event]

    data
}


get_roc <- function(data, response, label)
{
    response_prob <- paste0(response, "_prob")
    roc <- roc(response = data[[response]],
               predictor = data[[response_prob]])
    roc_test_tbl <- tibble(label = label,
                           sensitivities = roc$sensitivities,
                           specificities = roc$specificities)
}

hbr_minimal_dataset_test <- readRDS("gendata/hbr_minimal_dataset_test.rds")
hbr_minimal_dataset_train <- readRDS("gendata/hbr_minimal_dataset_train.rds")

## Get the predictors and response variable for the test and
## training set (all column except date; response is the last
## column
data_test <- hbr_minimal_dataset_test %>%
    select(-date)
data_train <- hbr_minimal_dataset_train %>%
    select(-date)

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM). 

## Remove zero-variance predictors from the test and train sets
predictors_train <- hbr_minimal_dataset_train %>%
    select(-bleed)
near_zero_var_indices <- nearZeroVar(predictors_train)
data_test <- data_test %>%
    select(-near_zero_var_indices)
data_train <- data_train %>%
    select(-near_zero_var_indices)

## Drop rows with missing values in the training and test sets
data_train <- data_train %>%
    drop_na()
message("Kept ", nrow(data_train), " rows out of ",
        nrow(hbr_minimal_dataset_train), " (missingness, train)")
data_test <- data_test %>%
    drop_na()
message("Kept ", nrow(data_test), " rows out of ",
        nrow(hbr_minimal_dataset_test), " (missingness, test)")


## TODO Deal with class inbalance here
## See this: "https://datascience.stackexchange.com/questions/82073/
## why-you-shouldnt-upsample-before-cross-validation"

## Randomness is used below this point (for the cross-validation)
set.seed(476)

## Fit the logistic regression model. There are no tuning parameters for
## logistic regression. Use cross-validation to assess average model
## performance within the training set
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     savePredictions = TRUE)
fit <- train(bleed ~ .,
             data = data_train,
             method = "glm",
             metric = "ROC",
             trControl = ctrl)

## View the summary, look for ROC area, which is the average
## over the n folds of the cross validation
fit
message("The SD of the AUC for the ROC is: ", fit$results$ROCSD)

roc_cv <- get_cv_roc(fit)
    
## Repredict the training dataset using the model
data_train <- data_train %>%
    add_predictions(fit, response = "bleed", positive_event = "bleed_occured")

roc_train <- data_train %>%
    get_roc(response = "bleed", label = "train")

## Use the model to make predictions on the test data, and record
## the class probabilities.
data_test <- data_test %>%
    add_predictions(fit, response = "bleed", positive_event = "bleed_occured")

roc_test <- data_test %>%
    get_roc(response = "bleed", label = "test")

## Combine all the ROC curves
roc_curves <- rbind(roc_train, roc_test, roc_cv) %>%
    mutate(type = case_when(label == "train" ~ "train",
                            label == "test" ~ "test",
                            TRUE ~ "fold"))

## Plot the ROC curves for each fold, the ROC curve for repredicting
## the training set, and the ROC curve for predicting the test set
ggplot(roc_curves, aes(x=specificities,y=sensitivities)) +
    geom_path(aes(group = label, colour = type)) +
    ylim(0,1) +
    geom_abline(aes(slope = 1, intercept = 1)) +
    scale_x_reverse(limit = c(1,0)) +
    scale_colour_manual(values = c("test"="green", "train"="red", "fold"="gray")) +
    theme_classic() +
    theme(legend.position = "bottom")

## Summary the performance
message("-- Summary of the model --")
fit
message("\n-- Model performance when repredicting training set --")
message("AUC (ROC) for full training set ", auc(roc_train))
ci(roc_train)
message("\n-- Model performance on test set --")
message("AUC (ROC) for test set ", auc(roc_test))
