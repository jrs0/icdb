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

    ## Predict using the test set
    bleed_pred <- fit$bleed %>%
        augment(test) %>%
        mutate(outcome = "bleed", pred_prob = .pred_bleed_occured,
               sample_num = sample_num) %>%
        mutate(truth = recode_factor(bleed_after, "bleed_occured" = "occured", "no_bleed" = "none"))

    ischaemia_pred <- fit$ischaemia %>%
        augment(test) %>%
        mutate(outcome = "ischaemia", pred_prob = .pred_ischaemia_occured,
               sample_num = sample_num) %>%
        mutate(truth = recode_factor(ischaemia_after, "ischaemia_occured" = "occured", "no_ischaemia" = "none"))

    pred <- bind_rows(bleed_pred, ischaemia_pred)

    
}


pred <- 1:100 %>%
    purrr::map(~ sample_and_fit(.x)) %>%
    purrr::list_rbind()

pred_factor <- pred %>%
    mutate(id = as.factor(id))


pred_factor %>%
    select(id, outcome, sample_num, pred_prob) %>%
    filter(sample_num == 1) %>%
    pivot_wider(names_from = outcome, values_from = pred_prob) %>%
    ggplot(aes(x = bleed, y = ischaemia)) +
    geom_point() +
    scale_y_log10() +
    scale_x_log10()
