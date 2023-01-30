##' Perform a logistic regression to attempt to predict bleeding outcome from the
##' minimal HBR dataset
##'
##' 

library(caret)
library(corrplot)
library(pROC)
library(tidyverse)

source("utils.R")

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

## Get the ROC curves for the models fitted in each fold
roc_cv <- get_cv_roc(fit)

## Get the (re-)predicted class probabilities for the training set,
## in order to ocmpute ROC curves
data_train <- data_train %>%
    add_prediction_probs(fit, response = "bleed", positive_event = "bleed_occured")

## Get the ROC curve for the training set reprediction
roc_train <- data_train %>%
    get_roc(response = "bleed", label = "train")

## Get the predicted class probabilities for the test set,
## in order to ocmpute ROC curves
data_test <- data_test %>%
    add_prediction_probs(fit, response = "bleed", positive_event = "bleed_occured")

## Store the ROC curve for the testing set prediction
roc_test <- data_test %>%
    get_roc(response = "bleed", label = "test")

## Combine all the ROC curves
roc_curves <- rbind(roc_train, roc_test, roc_cv) %>%
    mutate(type = case_when(label == "train" ~ "Train",
                            label == "test" ~ "Test",
                            TRUE ~ "Fold"))

## Plot the ROC curves for each fold, the ROC curve for repredicting
## the training set, and the ROC curve for predicting the test set
ggplot(roc_curves, aes(x=specificities,y=sensitivities)) +
    geom_path(aes(group = label, colour = type)) +
    ylim(0,1) +
    geom_abline(aes(slope = 1, intercept = 1)) +
    scale_x_reverse(limit = c(1,0)) +
    scale_colour_manual(values = c("Test"="green", "Train"="red", "Fold"="gray")) +
    theme_bw() +
    labs(title = "ROC curves for the test and train sets (including each cross-validation fold)",
         x = "Specificity", y = "Sensitivity") + 
    theme(legend.position = "bottom")


## Make predictions based on a particular manually chosen threshold
p_tr = 0.03
data_test_predict <- data_test %>%
    mutate(bleed_predict = cut(bleed_prob, breaks = c(0, p_tr, 1), labels = c("no_bleed", "bleed_occured")))
confusionMatrix(data_test_predict$bleed_predict, data_test_predict$bleed, "bleed_occured") 
