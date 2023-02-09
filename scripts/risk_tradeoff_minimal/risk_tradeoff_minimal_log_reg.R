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
summary(dataset)


set.seed(47)

## ======== Test/train split ===========

## Get a test training split stratified by bleeding (the
## less common outcome)
split <- initial_split(dataset, prop = 0.75, strata = bleed_after)
train <- training(split)
test <- testing(split)

## ========= Bleeding model ============

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM).
bleed_rec <- recipe(bleed_after ~ ., data = train) %>%
    update_role(date, new_role = "date") %>%
    update_role(ischaemia_after, new_role = "ischaemic_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())    
summary(bleed_rec)

## Specify a logistic regression model
bleed_model <- logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')

bleed_workflow <- 
    workflow() %>% 
    add_model(bleed_model) %>% 
    add_recipe(bleed_rec)

## Fit to training data
bleed_fit <- bleed_workflow %>%
    fit(data = train)

## View the fit
bleed_fit %>%
    extract_fit_parsnip() %>% 
    tidy()

## Predict using the test set
bleed_aug <- augment(bleed_fit, test)

## Plot the ROC curve
bleed_aug %>% 
  roc_curve(truth = bleed_after, .pred_bleed_occured) %>% 
  autoplot()

## ========= Ischaemia model ============

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM).
ischaemia_rec <- recipe(ischaemia_after ~ ., data = train) %>%
    update_role(date, new_role = "date") %>%
    update_role(bleed_after, new_role = "bleed_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())    
summary(ischaemia_rec)

## Specify a logistic regression model
ischaemia_model <- logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')

ischaemia_workflow <- 
    workflow() %>% 
    add_model(ischaemia_model) %>% 
    add_recipe(ischaemia_rec)

## Fit to training data
ischaemia_fit <- ischaemia_workflow %>%
    fit(data = train)

## View the fit
ischaemia_fit %>%
    extract_fit_parsnip() %>% 
    tidy()

## Predict using the test set
ischaemia_aug <- augment(ischaemia_fit, test)

## Plot the ROC curve
ischaemia_aug %>% 
    roc_curve(truth = ischaemia_after, .pred_ischaemia_occured) %>% 
    autoplot()

## ======== Combine predictions ============

combined <- test %>%
    mutate(.pred_bleed_occured = bleed_aug$.pred_bleed_occured) %>%
    mutate(.pred_ischaemia_occured = ischaemia_aug$.pred_ischaemia_occured)

## ======= Plot risk trade-off ===========

ggplot(combined, aes(x = .pred_bleed_occured, y = .pred_ischaemia_occured)) +
    geom_point()
