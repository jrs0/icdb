##' Use trees to predict bleeding
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

## Trees are more insensitive to predictor characteristics
## (0. 27 APM), so leave the predictors alone.

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
upsample = TRUE
if (upsample)
{
    data_train <- data_train %>%
        upSample(y = data_train$bleed, yname = "bleed_upsampled") %>%
        as_tibble() %>%
        select(-bleed) %>%
        rename(bleed = bleed_upsampled)
}

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

## Models you can insert here (replace the method parameter)
## - rpart (CART, using complexity tuning parameter)
## - rpart2 (CART, using max tree depth tuning parameter)
## - 
fit <- train(bleed ~ .,
             data = data_train,
             tuneLength = 30,
             method = "rpart2",
             metric = "ROC",
             trControl = ctrl)

## View the ROC metric as a function of the tuning parameter
plot(fit)

## View the summary, look for ROC area, which is the average
## over the n folds of the cross validation
fit

roc_cv <- get_cv_roc(fit)
    
## Repredict the training dataset using the model
data_train <- data_train %>%
    add_prediction_probs(fit, response = "bleed", positive_event = "bleed_occured")

## Get the ROC curve for the training set reprediction
roc_train <- data_train %>%
    get_roc(response = "bleed", label = "train")

## Use the model to make predictions on the test data, and record
## the class probabilities.
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

## Compute the "best" threshold value (ROC curve point closest to top level)
roc <- roc(data_test$bleed, data_test$bleed_prob)
p_tr <- coords(roc, x = "best", best.method = "closest.topleft")

## Make predictions based on a particular manually chosen threshold
data_test %>% print_confusion(bleed, bleed_prob, p_tr$threshold)
