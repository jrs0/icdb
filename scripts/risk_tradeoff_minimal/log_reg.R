##' The object of this script is to obtain confidence intervals
##' around the predicted probabilities.
##'
##' The idea of this script is to use the models developed in the
##' cross-validation resamples to give an indication of variation
##' in the outcome class score (the real number that is normally
##' used to make a class prediction). 
##'

library(tidymodels)
library(probably)
library(discrim)
library(probably)

## For attempt to find optimal threshold
library(pROC)

##' Using the cross-validation resamples to obtain multiple models
##' and thereby obtain a spectrum of predicted class scoress. The
##' intent is to assess the variability in the predicted probabilities.
##'
##' @return A tibble with the test set, containing predictors retained
##' in the model and the predicted outcome, along with a model_id column
##' indicating which cross-validation model was used to make the predictions.
##' @author 
predict_resample <- function(train, test, folds, recipe)
{
    model <- logistic_reg() %>% 
        set_engine('glm') %>% 
        set_mode('classification')
    workflow <- workflow() %>%
        add_model(model) %>%
        add_recipe(recipe)
    ctrl_rs <- control_resamples(
        extract = function (x) extract_fit_parsnip(x)
    )
    fits <- workflow %>%
        fit_resamples(folds, control = ctrl_rs) %>%
        pull(.extracts) %>%
        map(~ .x %>% pluck(".extracts", 1))
    ## Use all the models developed on each cross-validation fold
    ## to predict probabilities in the training set
    pre <- recipe %>%
        prep() %>%
        bake(new_data = test) %>%
        ## The id is necessary later to pair up predictions from
        ## multiple different calls to this function
        mutate(id = as.factor(row_number()))
    ## For each bleeding model, predict the probabilities for
    ## the test set and record the model used to make the
    ## prediction in model_id
    pred_bleed <- list(
        n = seq_along(fits),
        f = fits
    ) %>%
        pmap(function(n, f)
        {
            f %>%
                augment(new_data = pre) %>%
                mutate(model_id = as.factor(n))
        }) %>%
        list_rbind()
}
        
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

## Get a test training split stratified by bleeding (the
## less common outcome)
split <- initial_split(dataset, prop = 0.75, strata = bleed_after)
train <- training(split)
test <- testing(split)

## Create cross-validation folds
folds <- vfold_cv(train,
                  v = 50,
                  strata = bleed_after)

## Create the recipe for the bleed models
bleed_recipe <- recipe(bleed_after ~ ., data = train) %>%
    update_role(date, new_role = "date") %>%
    update_role(ischaemia_after, new_role = "ischaemic_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())

## Create the ischaemia recipe
ischaemia_recipe <- recipe(ischaemia_after ~ ., data = train) %>%
    update_role(date, new_role = "date") %>%
    update_role(bleed_after, new_role = "bleed_after") %>%
    step_integer(stemi_presentation) %>%
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())

## Perform the resampling predictions
pred_bleed <- predict_resample(train, test, folds, bleed_recipe)
pred_ischaemia <- predict_resample(train, test, folds, ischaemia_recipe)

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
