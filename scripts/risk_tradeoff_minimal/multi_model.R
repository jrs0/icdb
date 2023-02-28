##' Script that performs basic model fitting of multiple models
##' on the ICD HES data.
##'

library(tidymodels)
library(probably)
library(discrim)
library(probably)

## For attempt to find optimal threshold
library(pROC)

##' Using the bootstrap resamples to obtain multiple models
##' and thereby obtain a spectrum of predicted class scores for each
##' element of the test set. This indicates the variability in the
##' model fitting process.
##'
##' @param model The tidymodels model to use for the fit
##' @param train The training data on which to perform the fits
##' @param test The test set, where each model (from the bootstrap resamples)
##' is used to make a prediction for the items in the test set
##' @param resamples_from_train The bootstrap resamples object used to specify
##' which subsets of train are used to train the models.
##' @param recipe The recipe (preprocessing steps) to apply before training
##' _any_ of the models. (i.e. the same preprocessing steps are applied before
##' fitting the models in the bootstrap resamples.
##' @return A tibble with the test set, containing predictors retained
##' in the model and the predicted outcome, along with a model_id column
##' indicating which bootstrap resample was used to fit that model
##' 
predict_resample <- function(model, train, test, resamples_from_train, recipe)
{
    ## Create the workflow for this model
    workflow <- workflow() %>%
        add_model(model) %>%
        add_recipe(recipe)

    ## Set the control to extract the fitted model for each resample
    ## Turns out you don't need to run extract_fit_parsnip. Surely
    ## there is a way not to call the identity function!
    ctrl_rs <- control_resamples(
        extract = function (x) x
    )

    ## Perform an independent fit on each bootstrapped resample,
    ## extracting the fit objects
    bootstrap_fits <- workflow %>%
        fit_resamples(resamples_from_train, control = ctrl_rs) %>%
        pull(.extracts) %>%
        map(~ .x %>% pluck(".extracts", 1))

    ## Perform one fit on the entire training dataset. This is the
    ## single (no cross-validation here) main fit of the model on the
    ## entire training set.
    primary_fit <- workflow %>%
        fit(data = train)

    ## Use the primary model to predict the test set
    primary_pred <- primary_fit %>%
        augment(new_data = test) %>%
        mutate(model_id = as.factor("primary"))

    ## For each bleeding model, predict the probabilities for
    ## the test set and record the model used to make the
    ## prediction in model_id
    bootstrap_pred <- list(
        n = seq_along(bootstrap_fits),
        f = bootstrap_fits
    ) %>%
        pmap(function(n, f)
        {
            f %>%
                augment(new_data = test) %>%
                mutate(model_id = as.factor(n))
        }) %>%
        list_rbind()

    ## Bind together the primary and bootstrap fits
    pred <- bind_rows(primary_pred, bootstrap_pred)

    pred 
}
        
## Load the data and convert the bleeding outcome to a factor
## (levels no_bleed, bleed_occured). The result is a dataset with age and
## all the ICD <code>_before columns, and a response variable bleed.

risk_tradeoff_minimal_dataset <- readRDS("gendata/risk_tradeoff_minimal_dataset.rds")
dataset <- risk_tradeoff_minimal_dataset %>%
    mutate(bleed_after = factor(bleed_after == 0, labels = c("bleed_occured", "no_bleed"))) %>%
    mutate(ischaemia_after = factor(ischaemia_after == 0, labels = c("ischaemia_occured", "no_ischaemia"))) %>%
    drop_na() %>%
    ## Add an ID to link up patients between bleeding and ischaemia predictions
    mutate(id = as.factor(row_number()))
summary(dataset)

set.seed(47)

## Get a test training split stratified by bleeding (the
## less common outcome)
split <- initial_split(dataset, prop = 0.75, strata = bleed_after)
train <- training(split)
test <- testing(split)

## Create cross-validation folds
resamples_from_train <- bootstraps(train, times = 30)

## Create the model list
models <- list(
    ## Logistic regression
    log_reg = logistic_reg() %>% 
        set_engine('glm') %>% 
        set_mode('classification'),
    ## Linear discriminant analysis
    lin_disc = discrim_linear(
        mode = "classification",
        penalty = NULL,
        regularization_method = NULL,
        engine = "MASS"),
    ## Naive Bayes
    naive_bayes = naive_Bayes(
        mode = "classification",
        smoothness = NULL,
        Laplace = NULL,
        engine = "klaR"),
    ## Decision tree
    decision_tree = decision_tree() %>% 
        set_engine("rpart") %>% 
        set_mode("classification")
)

## Create the recipe for the bleed models
bleed_recipe <- recipe(bleed_after ~ ., data = train) %>%
    update_role(id, new_role = "id") %>%
    update_role(date, new_role = "date") %>%
    update_role(ischaemia_after, new_role = "ischaemic_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())

## Create the ischaemia recipe
ischaemia_recipe <- recipe(ischaemia_after ~ ., data = train) %>%
    update_role(id, new_role = "id") %>%
    update_role(date, new_role = "date") %>%
    update_role(bleed_after, new_role = "bleed_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())

## Perform the resampling predictions for all the models for bleeding
pred_bleed <- list(names(models), models) %>%
    purrr::pmap(function(model_name, model)
    {
        predict_resample(model, train, test,
                         resamples_from_train, bleed_recipe) %>%
            mutate(model_name = model_name) %>%
            mutate(outcome_name = "bleed") %>%
            mutate(outcome_result =
                       recode_factor(bleed_after,
                                     "bleed_occured" = "occured",
                                     "no_bleed" = "none")) %>%
            rename(.pred_occured = .pred_bleed_occured)
    }) %>%
    list_rbind()

## Perform the resampling predictions for all the models for ischaemia
pred_ischaemia <- list(names(models), models) %>%
    purrr::pmap(function(model_name, model)
    {
        predict_resample(model, train, test,
                         resamples_from_train, ischaemia_recipe) %>%
            mutate(model_name = model_name) %>%
            mutate(outcome_name = "ischaemia") %>%
            mutate(outcome_result =
                       recode_factor(ischaemia_after,
                                     "ischaemia_occured" = "occured",
                                     "no_ischaemia" = "none")) %>%
            rename(.pred_occured = .pred_ischaemia_occured)
    }) %>%
    list_rbind()

## Predict using the test set. Data needs to be in long
## format to be able to get at the individual ROC curves
## later
pred <- bind_rows(pred_bleed, pred_ischaemia) %>%
    mutate(primary = case_when(model_id == "primary" ~ "primary",
                               TRUE ~ "Bootstrap"))

## Plot a few example probabilities in the predicted data
## pred %>%
##     ## Uncomment to view one model for all patients
##     ##filter(model_name == "log_reg") %>%
##     ## Pick a single patients
##     filter(id == 3) %>%
##     ## Uncomment to view all models for some patients
##     filter(id %in% c(3,5,12)) %>%
##     ggplot(aes(x = .pred_bleed_occured,
##                y = .pred_ischaemia_occured,
##                color = model_name,
##                shape = primary)) +
##     geom_point() +
##     scale_y_log10() +
##     scale_x_log10()

## What do we want
## - Table containing: AUC of primary model; mean AUC of bootstrapped models
##   variance of AUC of bootstrapped models. Positive and negative predictive
##   value for each primary model, and mean and variance for bootstrap models.
##   Summary of the variance in the predictions for bleeding and ischaemia
##   risk for each model. Summary of the variance between the models for
##   the bleeding and ischaemia risk.
##
## - Two ROC curves for each model (one for bleeding, one for ischaemia).
##   Curves should show the primary model, and all the curves for the
##   bootstrapped models
##
## - Two plots per model (one for bleeding, one for ischaemia), showing
##   centered variance of all predicted probabilities
##
## - Several plots, with a few randomly selected patients, showing what
##   all the models predict for bleeding and ischaemia risk
##
## - Risk-tradeoff-style plots, one for each model, showing the distribution
##   of all the risk predictions
##
##

## Plot AUC curves
pred %>%
    filter(model_name == "log_reg") %>%
    group_by(model_name, outcome_name, primary, model_id) %>%
    roc_curve(outcome_result, .pred_occured) %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity,
           color=outcome_name, linetype = model_id)) +
    geom_line() +
    geom_abline(slope = 1, intercept = 0, size = 0.4) +
    coord_fixed() +
    labs(title = "ROC curves for each fitted model")

