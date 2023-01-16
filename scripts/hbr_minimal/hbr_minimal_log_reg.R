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

## Plot the average cross-validated ROC curve across folds,
## for re-predicting the training data.
## Look here: "https://stackoverflow.com/questions/37215366
## /plot-roc-curve-from-cross-validation-training-data-in-r"
plot(roc(predictor = fit$pred$bleed_occured, response = fit$pred$obs))

library(plyr)
l_ply(split(fit$pred, fit$pred$Resample), function(d) {
    plot(roc(predictor = d$bleed_occured, response = d$obs))
})

## From "https://stackoverflow.com/questions/69246553/
## plot-the-average-cross-validated-auc-from-caret-package"
##
## sapply roc(), bind as tibble with Resample as .id
dd.roc <- sapply(X = unique(fit$pred$Resample),
                 FUN = function(x) {
                     r <- fit$pred[fit$pred$Resample == x,]
                     R <- roc(response = r$obs, predictor = r$bleed_occured)
                     data.frame(sensitivities = R$sensitivities,
                                specificities = R$specificities)
                 }, simplify = F) %>%
    bind_rows(.id = "Resample") %>%
    as_tibble() %>%
    arrange(specificities)

d.roc <- roc(response = fit$pred$obs, predictor = fit$pred$bleed_occured)

ggplot(dd.roc, aes(x=specificities,y=sensitivities)) +
    ## geom_point(colour = "tomato", alpha = 0.1) +
    ## geom_density2d() +
    geom_path(aes(group = Resample, colour = "ROC per resample"), alpha = 0.1) +
    geom_smooth(colour = "tomato", size = 1.5) +
    ##    geom_line(data = d.roc, aes(colour = "ROC over all resamples"), size = 1.5) +
    ylim(0,1) +
    geom_abline(aes(slope = 1, intercept = 1)) +
    scale_x_reverse(limit = c(1,0)) +
    scale_colour_manual(values = c("seagreen","tomato"), name = "") +
    theme_classic() +
    theme(legend.position = "bottom")

## Use the model to make predictions on the test data, and record
## the class probabilities.
data_test <- data_test %>%
    mutate(bleed_prediction = predict(fit,
                                      newdata = data_test)) %>%
    mutate(bleed_prediction_prob = predict(fit,
                                      newdata = data_test,
                                      type = "prob")[,"bleed_occured"])
    
## Plot the ROC curve from the predicted probabilities
roc_curve <- roc(response = data_test$bleed,
                 predictor = data_test$bleed_prediction_prob)

## Show the area under the ROC curve with a confidence interval
auc(roc_curve)
ci(roc_curve)

## Plot the ROC curve
plot(roc_curve, legacy.axes = TRUE)

## Plot the confusion matrix 
confusionMatrix(data_test$bleed, data_test$bleed_prediction)
