##' Modelling the bleeding/ischaemic risk tradeoff using multiple classification
##' models, based on the minimal HES/ICD dataset

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
    mutate(ischaemia_after = factor(ischaemia_after == 0, labels = c("ischaemia_occured", "no_ischaemia")))
summary(dataset)

set.seed(47)

## ======== Test/train split ===========

## Get a test training split stratified by bleeding (the
## less common outcome)
split <- initial_split(dataset, prop = 0.75, strata = bleed_after)
train <- training(split)
test <- testing(split) %>%
    drop_na() %>%
    ## The id is necessary later to pair up bleeding/ischaemic predictions
    mutate(id = row_number())

## Create cross-validation folds
folds <- vfold_cv(train, v = 10)

tuning_plots <- list(
    ## For decision tree
    function(tuning_results)
    {
        tuning_results %>%
            collect_metrics() %>%
            mutate(tree_depth = factor(tree_depth)) %>%
            ggplot(aes(cost_complexity, mean, color = tree_depth)) +
            geom_line(linewidth = 1.5, alpha = 0.6) +
            geom_point(size = 2) +
            facet_wrap(~ .metric, scales = "free", nrow = 2) +
            scale_x_log10(labels = scales::label_number())
    }
)

## ========= Model selection =============

models <- list(
    ## Logistic regression
    ## log_reg = logistic_reg() %>% 
    ## set_engine('glm') %>% 
    ## set_mode('classification'),
    ## Linear discriminant analysis
    ## lin_disc = discrim_linear(
    ##     mode = "classification",
    ##     penalty = NULL,
    ##     regularization_method = NULL,
    ##     engine = "MASS"),
    ## Naive Bayes
    ## naive_bayes = naive_Bayes(
    ##     mode = "classification",
    ##     smoothness = NULL,
    ##     Laplace = NULL,
    ##     engine = "klaR"),
    ## Decision tree
    decision_tree <- 
        decision_tree(
            cost_complexity = tune(),
            tree_depth = tune()
        ) %>% 
        set_engine("rpart") %>% 
        set_mode("classification")
    ## Boosted trees
    ## boost_tree = boost_tree(
    ##     mode = "unknown",
    ##     engine = "xgboost",
    ##     mtry = NULL,
    ##     trees = NULL,
    ##     min_n = NULL,
    ##     tree_depth = NULL,
    ##     learn_rate = NULL,
    ##     loss_reduction = NULL,
    ##     sample_size = NULL,
    ##     stop_iter = NULL) %>%
    ## set_mode("classification"),
    ## Random forest
    ## rand_forest = rand_forest(
    ##     mode = "unknown",
    ##     engine = "ranger",
    ##     mtry = NULL,
    ##     trees = NULL,
    ##     min_n = NULL) %>%
    ## set_mode("classification")
)

## ======== Tuning grids ========

tune_grids <- list(
    ## Logistic regression
    ##grid_regular(),
    ## Linear discriminant analysis
    ##grid_regular(),
    ## Naive Bayes
    ##grid_regular(),
    ## Decision tree
    grid_regular(cost_complexity(),
                 tree_depth(),
                 levels = 5)
    ## Boosted trees
    ##grid_regular(),
    ## Random forest
    ##grid_regular()
)

## ========= Recipes ============

base_recipes = list(
    ## Logistic regression requires preprocessing of the predictors
    ## for sparse/unbalanced variables (p. 285, APM).
    bleed = recipe(bleed_after ~ ., data = train) %>%
        update_role(date, new_role = "date") %>%
        update_role(ischaemia_after, new_role = "ischaemic_after") %>%
        step_integer(stemi_presentation),
    ## Logistic regression requires preprocessing of the predictors
    ## for sparse/unbalanced variables (p. 285, APM).
    ischaemia = recipe(ischaemia_after ~ ., data = train) %>%
        update_role(date, new_role = "date") %>%
        update_role(bleed_after, new_role = "bleed_after") %>%
        step_integer(stemi_presentation))
    
## ======== Model specific recipes ============

specific_recipes = list(
    ## Logistic regression requires preprocessing of the predictors
    ## for sparse/unbalanced variables (p. 285, APM).
    ## base_recipes %>%
    ## purrr::map(~ .x %>%
    ##                step_integer(stemi_presentation) %>%
    ##                step_nzv(all_predictors()) %>%
    ##                step_center(all_predictors()) %>%
    ##                step_scale(all_predictors())),
    ## Linear discriminant analysis
    ## base_recipes %>%
    ## purrr::map(~ .x %>%
    ##                step_integer(stemi_presentation) %>%
    ##                step_nzv(all_predictors()) %>%
    ##                step_center(all_predictors()) %>%
    ##                step_scale(all_predictors()) %>%
    ##                step_naomit(all_predictors(), all_outcomes())),
    ## Naive Bayes
    ## base_recipes %>%
    ## purrr::map(~ .x %>%
    ##                step_integer(stemi_presentation) %>%
    ##                step_nzv(all_predictors()) %>%
    ##                step_center(all_predictors()) %>%
    ##                step_scale(all_predictors()) %>%
    ##                step_naomit(all_predictors(), all_outcomes())),
    ## Decision tree
    base_recipes %>%
    purrr::map(~ .x %>%
                   step_integer(stemi_presentation) %>%
                   step_nzv(all_predictors()) %>%
                   step_center(all_predictors()) %>%
                   step_scale(all_predictors()) %>%
                   step_naomit(all_predictors(), all_outcomes()))
    ## Boosted trees
    ## base_recipes %>%
    ## purrr::map(~ .x %>%
    ##                step_integer(stemi_presentation) %>%
    ##                step_nzv(all_predictors()) %>%
    ##                step_center(all_predictors()) %>%
    ##                step_scale(all_predictors()) %>%
    ##                step_naomit(all_predictors(), all_outcomes())),
    ## Random forest
    ## base_recipes %>%
    ## purrr::map(~ .x %>%
    ##                step_integer(stemi_presentation) %>%
    ##                step_nzv(all_predictors()) %>%
    ##                step_center(all_predictors()) %>%
    ##                step_scale(all_predictors()) %>%
    ##                step_naomit(all_predictors(), all_outcomes())))
)

## ========= Workflows ==========

## Each models (from the models list) has two recipes associated
## with it -- one for bleeding and one for ischaemia prediction
workflows <- list(models, specific_recipes) %>%
    purrr::pmap(function(model, recipes) {
        base_workflow <- workflow() %>%
            add_model(model)                
        list(
            bleed = base_workflow %>%
                add_recipe(recipes$bleed),
            ischaemia = base_workflow %>%
                add_recipe(recipes$ischaemia))
})

## ========= Fit the models =========

tuning_results <- list(workflows, tune_grids) %>%
    purrr::pmap(function(workflow, grid)
    {
        list(
            bleed = workflow$bleed %>% tune_grid(resamples = folds, grid = grid),
            ischaemia = workflow$ischaemia %>% tune_grid(resamples = folds, grid = grid)
        )
    })

## Plot the tuning results for each model
list(tuning_results, tuning_plots) %>%
    purrr::pmap(function(tune_result, tune_plot)
    {
        tune_plot(tune_result$bleed) + labs(title = "Tuning results for bleeding model")
        tune_plot(tune_result$bleed) + labs(title = "Tuning results for ischaemia model")
    })

## Select the best model by ROC curve

fits <- list(workflows, tuning_results) %>%
    purrr::pmap(function(workflow, tune_result)
    {
        bleed_best <- tune_result$bleed %>%
            select_best("roc_auc")
        bleed_fit <- workflow$bleed %>%
            finalize_workflow(bleed_best) %>%
            last_fit(split)
        ischaemia_best <- tune_result$ischaemia %>%
            select_best("roc_auc")
        ischaemia_fit <- workflow$ischaemia %>%
            finalize_workflow(ischaemia_best) %>%
            last_fit(split)
        list(
            bleed = bleed_fit,
            ischaemia = ischaemia_fit
        )
    })

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

## Calculate ROC curves
roc <- pred %>%
    group_by(model) %>%
    roc_curve(truth = truth, pred_prob)

## Plot the ROC curves
ggplot(roc, aes(x = 1 - specificity, y = sensitivity, color = model)) +
    geom_line() +
    geom_abline(slope = 1, intercept = 0, size = 0.4) +
    coord_fixed() +
    labs(title = "ROC curves for each fitted model")

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
