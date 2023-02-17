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
test <- testing(split) %>%
    ## The id is necessary later to pair up bleeding/ischaemic predictions
    mutate(id = row_number())

## Create cross-validation folds
folds <- vfold_cv(train, v = 10)

## ========= Model selection =============

model <- logistic_reg(
    penalty = tune(),
    mixture = tune()) %>% 
    set_engine('glmnet') %>% 
    set_mode('classification')

## ======== Tuning grids ========

grid <- grid_regular(penalty(), mixture())

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

tuning_results <- list(
    bleed = workflows$bleed %>%
        tune_grid(resamples = folds, grid = grid),
    ischaemia = workflows$ischaemia %>%
        tune_grid(resamples = folds, grid = grid))

## Combine metrics for plotting purposes
combined_metrics <- bind_rows(
    tuning_results$bleed %>% collect_metrics() %>% mutate(outcome = "bleed"),
    tuning_results$ischaemia %>% collect_metrics() %>% mutate(outcome = "ischaemia"))

## Plot the tuning results for each model
combined_metrics %>%
    mutate(mixture = factor(mixture)) %>%
    ggplot(aes(penalty, mean, color = mixture)) +
    geom_line(linewidth = 1.5, alpha = 0.6) +
    geom_point(size = 2) +
    facet_wrap(~ .metric + outcome, scales = "free", nrow = 2) +
    scale_x_log10(labels = scales::label_number()) +
    scale_color_viridis_d(option = "plasma", begin = .9, end = 0) +
    labs(title="Tuning results for bleeding and ischaemia models")


## Select the best model by ROC curve
bleed_best <- tuning_results$bleed %>%
    select_best("roc_auc")
bleed_fit <- workflows$bleed %>%
    finalize_workflow(bleed_best) %>%
    last_fit(split)
ischaemia_best <- tuning_results$ischaemia %>%
    select_best("roc_auc")
ischaemia_fit <- workflows$ischaemia %>%
    finalize_workflow(ischaemia_best) %>%
    last_fit(split)
 
## Combined the two models for plotting the ROC curves
combined_roc <- bind_rows(
    bleed_fit %>%
    collect_predictions() %>%
    mutate(outcome = "bleed", pred_prob = .pred_bleed_occured) %>%
    mutate(truth = recode_factor(bleed_after, "bleed_occured" = "occured", "no_bleed" = "none")),
    ischaemia_fit %>%
    collect_predictions() %>%
    mutate(outcome = "ischaemia", pred_prob = .pred_ischaemia_occured) %>%
    mutate(truth = recode_factor(ischaemia_after, "ischaemia_occured" = "occured", "no_ischaemia" = "none"))) %>%
    group_by(outcome) %>%
    roc_curve(truth, pred_prob)

## Plot the ROC curves
ggplot(combined_roc, aes(x = 1 - specificity, y = sensitivity, color = outcome)) +
    geom_line() +
    geom_abline(slope = 1, intercept = 0, size = 0.4) +
    coord_fixed() +
    labs(title = "ROC curves for each fitted model")

## Next step -- get the predictions =====






## Predict using the test set
pred <- list(fits, names(models)) %>%
    purrr::pmap(function(fit, model_name)
    {
        bleed_pred <- fit$bleed %>%
            augment(test) %>%
            mutate(outcome = "bleed", model = model_name, pred_prob = .pred_bleed_occured) %>%
            mutate(truth = recode_factor(bleed_after, "bleed_occured" = "occured", "no_bleed" = "none"))
        ischaemia_pred <- fit$ischaemia %>%
            augment(test) %>%
            mutate(outcome = "ischaemia", model = model_name, pred_prob = .pred_ischaemia_occured) %>%
            mutate(truth = recode_factor(ischaemia_after, "ischaemia_occured" = "occured", "no_ischaemia" = "none"))
        bind_rows(bleed_pred, ischaemia_pred)
    }) %>%
    purrr::list_rbind()





## Make a choice for a threshold (TODO not figured out how to
## do it in tidymodels yet)
bleed_threshold <- 0.03
ischaemia_threshold <- 0.05

## Repredict the classes based on the custom threshold
truth_levels <- levels(pred$truth)
pred_custom <- pred %>%
    mutate(pred = case_when(outcome == "bleed" ~ make_two_class_pred(
                                           pred_prob, truth_levels,
                                           threshold = bleed_threshold),
                            outcome == "ischaemia" ~ make_two_class_pred(
                                           pred_prob, truth_levels,
                                           threshold = ischaemia_threshold)))

## Using the custom prediction (based on the choice of
## threshold), compute model performance
multi_metrics <- metric_set(accuracy, kap, sens, spec, ppv, npv, roc_auc)
metrics <- pred_custom %>%
    group_by(outcome, model) %>%
    multi_metrics(truth = truth, pred_prob, estimate = pred)

## Plot AUC for the models
metrics %>%
    filter(.metric == "roc_auc") %>%
    ggplot(aes(x = reorder(model, -.estimate), y = .estimate, color = outcome)) +
    labs(title = "Area under ROC curve for each bleeding/ischaemia model",
         x = "Model", y = "AUC") +
    geom_point()

## Plot positive and negative predictive values
metrics %>%
    filter(stringr::str_detect(.metric, "ppv|npv")) %>%
    ggplot(aes(x = reorder(model, -.estimate), y = .estimate, color = outcome, shape = .metric)) +
    labs(title = "Positive and negative predictive values",
         x = "Model", y = "P") +
    geom_point()

## Plot sensitivities and specifities
metrics %>%
    filter(stringr::str_detect(.metric, "spec|sens")) %>%
    ggplot(aes(x = reorder(model, -.estimate), y = .estimate, color = outcome, shape = .metric)) +
    labs(title = "Sensitivies and specifities",
         x = "Model", y = "P") +
    geom_point()


## Plot the calibration plot
## bleed_aug_custom %>%
##     cal_plot_breaks(bleed_after, .pred_bleed_occured, num_breaks = 10)
                    
## Plot the calibration plot
## ischaemia_aug %>%
##     cal_plot_breaks(ischaemia_after, .pred_ischaemia_occured, num_breaks = 10)

## ======= Plot risk trade-off ===========


pred %>%
    select(id, outcome, model, pred_prob) %>%
    pivot_wider(names_from = outcome, values_from = pred_prob) %>%
    ggplot(aes(x = bleed, y = ischaemia, color = model)) +
    geom_point()
