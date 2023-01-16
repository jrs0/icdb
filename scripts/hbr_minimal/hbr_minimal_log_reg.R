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

## From "https://stackoverflow.com/questions/69246553/
## plot-the-average-cross-validated-auc-from-caret-package"
##
## sapply roc(), bind as tibble with Resample as .id
roc_cv <- sapply(X = unique(fit$pred$Resample),
                 FUN = function(x) {
                     r <- fit$pred[fit$pred$Resample == x,]
                     R <- roc(response = r$obs, predictor = r$bleed_occured)
                     data.frame(sensitivities = R$sensitivities,
                                specificities = R$specificities)
                 }, simplify = F) %>%
    bind_rows(.id = "Resample") %>%
    as_tibble() %>%
    arrange(specificities)

## Repredict the training dataset using the model
data_train <- data_train %>%
    mutate(bleed_prediction = predict(fit,
                                      newdata = data_train)) %>%
    mutate(bleed_prediction_prob = predict(fit,
                                           newdata = data_train,
                                           type = "prob")[,"bleed_occured"])

roc_train <- roc(response = data_train$bleed,
                 predictor = data_train$bleed_prediction_prob)
roc_train_tbl <- tibble(Resample = "train",
                        sensitivities = roc_train$sensitivities,
                        specificities = roc_train$specificities)


## Show the area under the ROC curve with a confidence interval
## For repredicting the training set
auc(roc_train)
ci(roc_train)

## Use the model to make predictions on the test data, and record
## the class probabilities.
data_test <- data_test %>%
    mutate(bleed_prediction = predict(fit,
                                      newdata = data_test)) %>%
    mutate(bleed_prediction_prob = predict(fit,
                                      newdata = data_test,
                                      type = "prob")[,"bleed_occured"])

roc_test <- roc(response = data_test$bleed,
                predictor = data_test$bleed_prediction_prob)
roc_test_tbl <- tibble(Resample = "test",
                       sensitivities = roc_test$sensitivities,
                       specificities = roc_test$specificities)

## Show the area under the ROC curve with a confidence interval
## For repredicting the test set
auc(roc_test)
ci(roc_test)

## Combine all the ROC curves
roc_curves <- rbind(roc_train_tbl, roc_test_tbl, roc_cv) %>%
    mutate(type = case_when(Resample == "train" ~ "train",
                            Resample == "test" ~ "test",
                            TRUE ~ "fold"))

## Plot the ROC curves for each fold, the ROC curve for repredicting
## the training set, and the ROC curve for predicting the test set
ggplot(roc_curves, aes(x=specificities,y=sensitivities)) +
    geom_path(aes(group = Resample, colour = type)) +
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
