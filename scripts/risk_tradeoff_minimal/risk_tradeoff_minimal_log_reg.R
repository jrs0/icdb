##' Perform a logistic regression to attempt to predict bleeding outcome from the
##' risk_tradeoff_minimal_dataset
##'
##' 

library(tidymodels)

## Load the data and convert the bleeding outcome to a factor
## (levels no_bleed, bleed_occured). The result is a dataset with age and
## all the ICD <code>_before columns, and a response variable bleed.
risk_tradeoff_minimal_dataset <- readRDS("gendata/risk_tradeoff_minimal_dataset.rds")
dataset <- risk_tradeoff_minimal_dataset %>%
    mutate(bleed_after = factor(bleed_after == 0, labels = c("bleed_occured", "no_bleed")))

set.seed(47)

## Create the training and test data
dataset_split <- initial_split(dataset, prop = 0.75,
                               strata = bleed_after)
dataset_train <- training(dataset_split)
dataset_test <- testing(dataset_split)

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM).
dataset_rec <- recipe(bleed_after ~ ., data = dataset_train) %>%
    update_role(date, new_role = "date") %>%
    update_role(ischaemia_after, new_role = "ischaemic_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())    
summary(dataset_rec)

## Specify a logistic regression model
lr_model <- logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')

dataset_workflow <- 
    workflow() %>% 
    add_model(lr_model) %>% 
    add_recipe(dataset_rec)

## Fit to training data
dataset_fit <- dataset_workflow %>%
    fit(data = dataset_train)

## View the fit
dataset_fit %>%
    extract_fit_parsnip() %>% 
    tidy()

## Predict using the test dataset
dataset_aug <- augment(dataset_fit, dataset_test)

## Plot the ROC curve
dataset_aug %>% 
  roc_curve(truth = bleed_after, .pred_bleed_occured) %>% 
  autoplot()
