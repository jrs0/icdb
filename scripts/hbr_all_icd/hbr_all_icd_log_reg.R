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
    select(-date, -matches("_after$"))


# Create the training and test data
dataset_split <- initial_split(dataset, prop = 0.75,
                               strata = bleed)
dataset_train <- dataset_split %>% 
    training()
dataset_test <- dataset_split %>% 
    testing()

# Specify a logistic regression model
logistic_model <- logistic_reg() %>% 
    set_engine('glm') %>% 
    set_mode('classification')

# Fit to training data
logistic_fit <- logistic_model %>% 
    fit(bleed ~ ., data = dataset_train)

# Print model fit object
logistic_fit
