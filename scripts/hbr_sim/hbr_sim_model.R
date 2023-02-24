## This script uses the simulated hbr_sim_dataset to assess the
## ability of the bleeding 

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


## Obtain the dataset
hbr_sim_dataset <- readRDS("gendata/hbr_sim_dataset.R")

dataset <- hbr_sim_dataset %>%
    mutate(bleed_after = factor(bleed == 0, labels = c("bleed_occured", "no_bleed"))) %>%
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
    step_nzv(all_predictors()) %>%
    step_center(all_predictors()) %>%
    step_scale(all_predictors())

## Perform the resampling predictions
pred_bleed <- predict_resample(train, test, folds, bleed_recipe)

