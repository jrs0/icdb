##' The object of this script is to obtain confidence intervals
##' around the predicted probabilities

library(tidymodels)
library(probably)
library(discrim)
library(probably)

## For attempt to find optimal threshold
library(pROC)

        
## Load the data and convert the bleeding outcome to a factor
## (levels no_bleed, bleed_occured). The result is a dataset with age and
## all the ICD <code>_before columns, and a response variable bleed.

risk_tradeoff_minimal_dataset <- readRDS("gendata/risk_tradeoff_minimal_dataset.rds")
dataset <- risk_tradeoff_minimal_dataset %>%
    mutate(bleed_after = factor(bleed_after == 0, labels = c("bleed_occured", "no_bleed"))) %>%
    mutate(ischaemia_after = factor(ischaemia_after == 0, labels = c("ischaemia_occured", "no_ischaemia"))) %>%
    drop_na()
summary(dataset)

set.seed(47)


## ======== Test/train split ===========

## Get a test training split stratified by bleeding (the
## less common outcome)
split <- initial_split(dataset, prop = 0.75, strata = bleed_after)
train <- training(split)
test <- testing(split)

## Create cross-validation folds
folds <- vfold_cv(train,
                  v = 100,
                  strata = bleed_after)

## ========= Model selection =============

model <- logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')

## ========= Recipes ============

recipes = list(
    ## Logistic regression requires preprocessing of the predictors
    ## for sparse/unbalanced variables (p. 285, APM).
    bleed = recipe(bleed_after ~ ., data = train) %>%
        update_role(date, new_role = "date") %>%
        update_role(ischaemia_after, new_role = "ischaemic_after") %>%
        step_integer(stemi_presentation) %>%
        step_nzv(all_predictors()) %>%
        step_center(all_predictors()) %>%
        step_scale(all_predictors()),
    ## Logistic regression requires preprocessing of the predictors
    ## for sparse/unbalanced variables (p. 285, APM).
    ischaemia = recipe(ischaemia_after ~ ., data = train) %>%
        update_role(date, new_role = "date") %>%
        update_role(bleed_after, new_role = "bleed_after") %>%
        step_integer(stemi_presentation) %>%
        step_nzv(all_predictors()) %>%
        step_center(all_predictors()) %>%
        step_scale(all_predictors()))

## ========= Workflows ==========

## Each models (from the models list) has two recipes associated
## with it -- one for bleeding and one for ischaemia prediction
base_workflow <- workflow() %>%
    add_model(model)
workflows <- list(
    bleed = base_workflow %>%
        add_recipe(recipes$bleed),
    ischaemia = base_workflow %>%
        add_recipe(recipes$ischaemia))

## Fit the models
ctrl_rs <- control_resamples(
    extract = function (x) extract_fit_parsnip(x)
)

fit_rs <- list(
    bleed = workflows$bleed %>%
        fit_resamples(folds, control = ctrl_rs),
    ischaemia = workflows$ischaemia %>%
        fit_resamples(folds, control = ctrl_rs))

fit_bleed <- fit_rs$bleed %>%
    pull(.extracts)
fit_ischaemia <- fit_rs$ischaemia %>%
    pull(.extracts)

## Use all the models developed on each cross-validation fold
## to predict probabilities in the training set
pre_bleed <- recipes$bleed %>%
    prep() %>%
    bake(new_data = test) %>%
    ## The id is necessary later to pair up bleeding/ischaemic predictions
    mutate(id = as.factor(row_number()))

## For each bleeding model, predict the probabilities for
## the test set and record the model used to make the
## prediction in model_id
bleed_fits <- fit_bleed %>%
    map(~ .x %>% pluck(".extracts", 1))

pred_bleed <- list(
    n = seq_along(bleed_fits),
    f = bleed_fits
) %>%
    pmap(function(n, f)
    {
        f %>%
            augment(new_data = pre_bleed) %>%
            mutate(model_id = as.factor(n))
    }) %>%
    list_rbind()

pre_ischaemia <- recipes$ischaemia %>%
    prep() %>%
    bake(new_data = test) %>%
    ## The id is necessary later to pair up bleeding/ischaemic predictions
    mutate(id = as.factor(row_number()))

ischaemia_fits <- fit_ischaemia %>%
    map(~ .x %>% pluck(".extracts", 1))

pred_ischaemia <- list(
    n = seq_along(ischaemia_fits),
    f = ischaemia_fits
) %>%
    pmap(function(n, f)
    {
        f %>%
            augment(new_data = pre_ischaemia) %>%
            mutate(model_id = as.factor(n))
    }) %>%
    list_rbind()

## Predict using the test set. Data is in wide format,
## with the bleeding and ischaemia predictions
pred <- pred_bleed %>%
    left_join(pred_ischaemia, by=c("id", "model_id"))

## Plot a few example probabilities in the predicted data
pred %>%
    ## Uncomment to view one model for all patients
    ##filter(sample_num == 1) %>%
    ## Uncomment to view all models for some patients
    filter(id %in% c(1,20,23,43, 100, 101, 102)) %>%
    ggplot(aes(x = .pred_bleed_occured,
               y = .pred_ischaemia_occured,
               color = id)) +
    geom_point() +
    scale_y_log10() +
    scale_x_log10()

## Compute the distributions 
