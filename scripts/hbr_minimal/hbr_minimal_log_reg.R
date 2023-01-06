##' Perform a logistic regression to attempt to predict bleeding outcome from the
##' minimal HBR dataset
##'
##' 

library(tidyverse)
library(caret)

hbr_minimal_dataset <- readRDS("gendata/hbr_minimal_dataset.rds")

ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)

levels(hbr_minimal_dataset$bleed)

## Drop the date from the predictors
dataset <- hbr_minimal_dataset %>%
    select(-date)

model_fit <- glm(bleed ~ .,
                 ## Select the rows for the pre-2008 data:
                 data = dataset,
                 ## 'family' relates to the distribution of the data.
                 ## A value of 'binomial' is used for logistic regression
                 family = binomial)
