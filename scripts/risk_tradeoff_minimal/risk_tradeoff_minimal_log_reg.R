##' Perform a logistic regression to attempt to predict bleeding outcome from the
##' risk_tradeoff_minimal_dataset
##'
##' 

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
    drop_na()

## ========= Model selection =============

models <- list(
    logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification'),

    discrim_linear(
        mode = "classification",
        penalty = NULL,
        regularization_method = NULL,
        engine = "MASS"
    )

    naive_Bayes(
        mode = "classification",
        smoothness = NULL,
        Laplace = NULL,
        engine = "klaR"
    )
    
    boost_tree(
        mode = "unknown",
        engine = "xgboost",
        mtry = NULL,
        trees = NULL,
        min_n = NULL,
        tree_depth = NULL,
        learn_rate = NULL,
        loss_reduction = NULL,
        sample_size = NULL,
        stop_iter = NULL) %>%
    set_mode("classification")
    
    rand_forest(
        mode = "unknown",
        engine = "ranger",
        mtry = NULL,
        trees = NULL,
        min_n = NULL) %>%
    set_mode("classification")

## ========= Bleeding model ============

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM).
bleed_rec <- recipe(bleed_after ~ ., data = train) %>%
    update_role(date, new_role = "date") %>%
    update_role(ischaemia_after, new_role = "ischaemic_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_naomit(all_predictors(), all_outcomes())
summary(bleed_rec)

bleed_workflow <- 
    workflow() %>% 
    add_model(model) %>% 
    add_recipe(bleed_rec)

## Fit to training data
bleed_fit <- bleed_workflow %>%
    fit(data = train)

## View the fit
bleed_fit %>%
    extract_fit_parsnip() %>%
    ## For some reason tidy() does not work always
    ##tidy()
    identity()

## Predict using the test set
bleed_aug <- augment(bleed_fit, test)

## Compute the ROC curve
bleed_roc <- bleed_aug %>%
    roc_curve(truth = bleed_after, .pred_bleed_occured)

## Plot the ROC curve
autoplot(bleed_roc)

## Make a choice for a threshold (TODO not figured out how to
## do it in tidymodels yet)
threshold <- 0.03
##bleed_roc %>%
##coords(x = "best", best.method = "closest.topleft")

## Repredict the classes based on the custom threshold
bleed_aug_custom <- bleed_aug %>%
    mutate(.pred_class = make_two_class_pred(.pred_bleed_occured, 
                                             levels(bleed_aug$bleed_after),
                                             threshold = threshold),
       .pred_class = factor(.pred_class, levels = levels(bleed_aug$bleed_after)))

## Get the AUC
bleed_auc <- bleed_fit %>%
    roc_auc(truth = bleed_after, .pred_bleed_occured)

bleed_aug_custom %>%
conf_mat(truth = bleed_after, estimate = .pred_class)

## Get summary stats for the fit
bleed_accuracy <- bleed_aug_custom %>%
    accuracy(truth = bleed_after, estimate = .pred_class)
bleed_sensitivity <- bleed_aug_custom %>%
    sens(truth = bleed_after, estimate = .pred_class)
bleed_specificity <- bleed_aug_custom %>%
    spec(truth = bleed_after, estimate = .pred_class)
bleed_ppv <- bleed_aug_custom %>%
    ppv(truth = bleed_after, estimate = .pred_class)
bleed_npv <- bleed_aug_custom %>%
    npv(truth = bleed_after, estimate = .pred_class)


## Get kappa for a bleeding prediction attempt
bleed_kappa <- bleed_aug %>%
    kap(truth = bleed_after, estimate = .pred_class)

## Plot the calibration plot
bleed_aug %>%
    cal_plot_breaks(bleed_after, .pred_bleed_occured, num_breaks = 10)
                    
## ========= Ischaemia model ============

## Logistic regression requires preprocessing of the predictors
## for sparse/unbalanced variables (p. 285, APM).
ischaemia_rec <- recipe(ischaemia_after ~ ., data = train) %>%
    update_role(date, new_role = "date") %>%
    update_role(bleed_after, new_role = "bleed_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors()) %>%
    step_naomit(all_predictors(), all_outcomes())
summary(ischaemia_rec)

ischaemia_workflow <- 
    workflow() %>% 
    add_model(model) %>% 
    add_recipe(ischaemia_rec)

## Fit to training data
ischaemia_fit <- ischaemia_workflow %>%
    fit(data = train)

## View the fit
ischaemia_fit %>%
    extract_fit_parsnip() %>% 
    ## tidy() %>%
    identity()
    
## Predict using the test set
ischaemia_aug <- augment(ischaemia_fit, test)

## Plot the ROC curve
ischaemia_aug %>% 
    roc_curve(truth = ischaemia_after, .pred_ischaemia_occured) %>% 
    autoplot()

## Get the AUC
ischaemia_auc <- ischaemia_aug %>%
    roc_auc(truth = ischaemia_after, .pred_ischaemia_occured)

## Plot the calibration plot
ischaemia_aug %>%
    cal_plot_breaks(ischaemia_after, .pred_ischaemia_occured, num_breaks = 10)

## ======== Combine predictions ============

combined <- test %>%
    mutate(.pred_bleed_occured = bleed_aug$.pred_bleed_occured) %>%
    mutate(.pred_ischaemia_occured = ischaemia_aug$.pred_ischaemia_occured)

## ======= Plot risk trade-off ===========

ggplot(combined, aes(x = .pred_bleed_occured, y = .pred_ischaemia_occured)) +
    geom_point()
