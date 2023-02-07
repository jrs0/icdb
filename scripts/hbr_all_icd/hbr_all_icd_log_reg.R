##' Apply the logistic regression model to subsequent bleeding events following
##' an ACS, as a function of prior ICD codes in the 12 months prior to the ACS
##' (each treated as a separate predictor).
##'
##'
##' 

library(tidymodels)

## Load the data, remove the _after variables (which are not relevant
## for this script), and convert the bleeding outcome to a factor
## (levels no_bleed, bleed_occured). The result is a dataset with age and
## all the ICD <code>_before columns, and a response variable bleed.
hbr_all_icd_dataset <- readRDS("gendata/hbr_all_icd_dataset.rds")
dataset <- hbr_all_icd_dataset %>%
    mutate(bleed = factor(bleed_after == 0, labels = c("no_bleed", "bleed_occured"))) %>%
    select(-bleed_after, -matches("_after$"))

set.seed(47)

## Create the training and test data
dataset_split <- initial_split(dataset, prop = 0.75,
                               strata = bleed)
dataset_train <- training(dataset_split)
dataset_test <- testing(dataset_split)

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM).
dataset_rec <- recipe(bleed ~ ., data = dataset_train) %>%
    update_role(date, new_role = "date") %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())    

# Specify a logistic regression model
lr_model <- logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')

dataset_workflow <- 
    workflow() %>% 
    add_model(lr_model) %>% 
    add_recipe(dataset_rec)

# Fit to training data
dataset_fit <- dataset_workflow %>%
    fit(data = dataset_train)

# Print model fit object
logistic_fit
