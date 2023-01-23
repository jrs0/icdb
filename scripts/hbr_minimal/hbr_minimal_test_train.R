##' Create a test-train split within the hbr_minimal dataset. The
##' split takes the most recent 20% of the data as the test set.
##' Data is not sampled within the most recent 20%. The assumption
##' is that the characteristics of the dataset have not changed
##' substantially over time.

library(tidyverse)

## Test set proportion (what proportion of the data to use in the
## test set)
prop <- 0.2

hbr_minimal_dataset <- readRDS("gendata/hbr_minimal_dataset.rds")

n <- nrow(hbr_minimal_dataset)

## Create the test set
hbr_minimal_dataset_test <- hbr_minimal_dataset %>%
    dplyr::arrange(desc(date)) %>%
    filter(row_number() <= n * prop)

## Create the training set
hbr_minimal_dataset_train <- hbr_minimal_dataset %>%
    dplyr::arrange(desc(date)) %>%
    filter(row_number() > n * prop)

## Save the datasets
saveRDS(hbr_minimal_dataset_test, "gendata/hbr_minimal_dataset_test.rds")
saveRDS(hbr_minimal_dataset_train, "gendata/hbr_minimal_dataset_train.rds")
