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
    mutate(bleed_after = factor(bleed_after == 0, labels = c("bleed_occured", "no_bleed"))) %>%
    mutate(ischaemia_after = factor(ischaemia_after == 0, labels = c("ischaemia_occured", "no_ischaemia")))

set.seed(47)

## ======= Bleeding model =======

## Create the training and test data
bleeding_split <- initial_split(dataset, prop = 0.75,
                               strata = bleed_after)
bleeding_train <- training(bleeding_split)
bleeding_test <- testing(bleeding_split)

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM).
bleeding_rec <- recipe(bleed_after ~ ., data = bleeding_train) %>%
    update_role(date, new_role = "date") %>%
    update_role(ischaemia_after, new_role = "ischaemic_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())    
summary(bleeding_rec)

## Specify a logistic regression model
lr_model <- logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')

bleeding_workflow <- 
    workflow() %>% 
    add_model(lr_model) %>% 
    add_recipe(bleeding_rec)

## Fit to training data
bleeding_fit <- bleeding_workflow %>%
    fit(data = bleeding_train)

## View the fit
bleeding_fit %>%
    extract_fit_parsnip() %>% 
    tidy()

## Predict using the test bleeding
bleeding_aug <- augment(bleeding_fit, bleeding_test)

## Plot the ROC curve
bleeding_aug %>% 
  roc_curve(truth = bleed_after, .pred_bleed_occured) %>% 
  autoplot()

## ======= Ischaemia model =======

## Create the training and test data
ischaemia_split <- initial_split(dataset, prop = 0.75,
                                 strata = ischaemia_after)
ischaemia_train <- training(ischaemia_split)
ischaemia_test <- testing(ischaemia_split)

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM).
ischaemia_rec <- recipe(ischaemia_after ~ ., data = ischaemia_train) %>%
    update_role(date, new_role = "date") %>%
    update_role(bleed_after, new_role = "bleed_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())    
summary(ischaemia_rec)

## Specify a logistic regression model
lr_model <- logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')

ischaemia_workflow <- 
    workflow() %>% 
    add_model(lr_model) %>% 
    add_recipe(ischaemia_rec)

## Fit to training data
ischaemia_fit <- ischaemia_workflow %>%
    fit(data = ischaemia_train)

## View the fit
ischaemia_fit %>%
    extract_fit_parsnip() %>% 
    tidy()

## Predict using the test ischaemia
ischaemia_aug <- augment(ischaemia_fit, ischaemia_test)

## Plot the ROC curve
ischaemia_aug %>% 
    roc_curve(truth = ischaemia_after, .pred_ischaemia_occured) %>% 
    autoplot()
