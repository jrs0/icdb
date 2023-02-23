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


sample_and_fit <- function(sample_num)
{

    ## ======== Test/train split ===========

    ## Get a test training split stratified by bleeding (the
    ## less common outcome)
    split <- initial_split(dataset, prop = 0.75, strata = bleed_after)
    train <- training(split)
    test <- testing(split) %>%
        ## The id is necessary later to pair up bleeding/ischaemic predictions
        mutate(id = row_number())

    ## Create cross-validation folds
    folds <- vfold_cv(train, v = 10)

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

    ## ========= Fit the models =========

    fit <- list(
        bleed = workflows$bleed %>%
            fit(data = train),
        ischaemia = workflows$ischaemia %>%
            fit(data = train))

    pred_bleed <- fit$bleed %>% augment(test)
    pred_ischaemia <- fit$ischaemia %>% augment(test)
    
    ## Predict using the test set. Data is in wide format,
    ## with the bleeding and ischaemia predictions
    pred_bleed %>%
        left_join(pred_ischaemia, by=c("id"="id"))    
}


pred <- 1:12 %>%
    purrr::map(~ sample_and_fit(.x)) %>%
    purrr::list_rbind() %>%
    mutate(id = as.factor(id))


pred %>%
    ## Uncomment to view one model for all patients
    ##filter(sample_num == 1) %>%
    ## Uncomment to view all models for some patients
    filter(id %in% c(1,20,23,43)) %>%
    ggplot(aes(x = .pred_bleed_occured,
               y = .pred_ischaemia_occured,
               color = id)) +
    geom_point() +
    scale_y_log10() +
    scale_x_log10()
