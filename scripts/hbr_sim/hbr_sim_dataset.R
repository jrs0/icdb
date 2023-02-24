##' This script creates a simulated dataset of high bleeding risk
##' based on the numbers in the paper "2020 Cao et al. - Validation of the
##' Academic Research Consortium High Bleeding Risk Definition in
##' Contemporary PCI Patients". The purpose is to provide a base level of
##' understanding of the performance of bleeding risk simulation in the
##' case where the exact risk is known.
##'
##' NOTE: this script does not work. Needs fixing.

library(tidyverse)
library(fabricatr)
library(GGally)
library(ggplot2)
library(dplyr)
library(purrr)

## Based on ARC-HBR validation paper (Cao et al.)

## Size of output / final dataset
n = 1e5

## Proportion of population at high bleeding risk (HBR)
p_hbr <- 0.444

## Proportion of the HBR population at different bleeding risks
## The elements correspond to the probability of arc score
## 1, 2, 3, 4
p_arc_hbr = c(
    0.616,
    0.291,
    0.079,
    0.014
)

## Define the proportions of HBR patients to be in each HBR score category - central illustration figure
p_hbr_criteria = list(
    major_sev_anaemia = 0.332,
    major_oac = 0.185,
    major_malig = 0.168,
    major_sev_ckd = 0.137,
    major_surgery = 0.082,
    major_thrmcyt = 0.043,
    minor_age = 0.468,
    minor_mod_ckd = 0.396,
    minor_mild_anaemia = 0.369,
    minor_cva = 0.207,
    minor_bleed = 0.037
)

## Probability of bleed occurance for the baseline
## population (baseline bleeding risk), vs probability
## of bleed occurance for HBR
p_bleed_base <- 0.032

## Hazard ratios due to presence of multiple ARC HBR
## criteria. Each value in this vector represents the
## hazard ration of having that ARC HBR "score". The
## score is defined by adding up major criteria as
## 1 and minor criteria as 0.5. (See Central Illustration
## in paper)
arc_hr <- tribble(
    ~arc_score, ~hr,
     0,          1,
     1,          2.1,
     2,          3.88,
     3,          6.98,
     4,          12.26,
)

## Add the actual risk score based on the baseline
## bleeding risk
arc_risk <- arc_hr %>%
    mutate(risk = hr * p_bleed_base) %>%
    mutate(excess_risk = risk - p_bleed_base) %>%
    mutate(arc_score_squared = arc_score^2)

quadratic_risk_model <- lm(excess_risk ~ 0 + arc_score_squared, data = arc_risk)
arc_risk$excess_risk_prediction <- predict(quadratic_risk_model)

## Show the fit between the model and the excess risk
ggplot(data=arc_risk, mapping=aes(x=arc_score)) +
    geom_point(aes(y=excess_risk)) +
    geom_smooth(aes(y=excess_risk_prediction),
                method = 'lm',
                formula = y ~ poly(x, 2))

## To use the model to predict risk, call this function
## arc_score here is 
estimate_risk <- function(arc_score) {
    newdata <- tibble(
        arc_score_squared = arc_score^2
    )
    ## Predict the excess risk, and add the baseline
    ## risk to get the risk estimate
    p_bleed_base + predict(quadratic_risk_model, newdata=newdata)
}

## Size of dataframe to sample from
n_sample = n*10

set.seed(470)

## Create the data
hbr_data <- tibble(
    ## MAJOR criteria - from figure 1
    major_sev_anaemia = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["major_sev_anaemia"]]),
    major_oac = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["major_oac"]]),
    major_malig = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["major_malig"]]),
    major_sev_ckd = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["major_sev_ckd"]]),
    major_surgery = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["major_surgery"]]),
    major_thrmcyt = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["major_thrmcyt"]]),
    ## MINOR criteria - from figure 1
    minor_age = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["minor_age"]]),
    minor_mod_ckd = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["minor_mod_ckd"]]),
    minor_mild_anaemia = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["minor_mild_anaemia"]]),
    minor_cva = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["minor_cva"]]),
    minor_bleed = rbinom(
        n = n_sample, size = 1,
        prob = p_hbr_criteria[["minor_bleed"]])) %>%
    ## Calculate the number of times the HBR category
    ## has been satisfied (1 for a major and 0.5 for minor)
    mutate(maj_score = rowSums(select(., starts_with("major"))),
           min_score = rowSums(select(., starts_with("minor"))) %/% 2,
           arc_hbr_score = map2_dbl(maj_score, min_score, sum)) %>%
    ## Remove non-HBR rows 
    filter(arc_hbr_score != 0) %>%
    ## Remove high HBR score. Use the length of
    ## p_arc_hbr to get the upper limit
    filter(arc_hbr_score <= length(p_arc_hbr)) %>%
    ## apply the wanted proportions of each HBR score group
    group_by(arc_hbr_score) %>%
    ## This is slice_head with prop = p_arc_hbr[[arc_hbr_score]]
    slice(as.integer(p_arc_hbr[[first(arc_hbr_score)]] * n())) %>%
    ungroup()

## Compute the proportion of HBR vs. non-HBR rows
n_hbr <- nrow(hbr_data)
n_non_hbr <- n - n_hbr

## Insert zero rows to make up proportion of non-HBR
n_cols <- hbr_data %>% ncol()
mat <- matrix(integer(n_non_hbr * n_cols), nrow = n_non_hbr)
non_hbr_data <- as_tibble(mat)
names(non_hbr_data) <- hbr_data %>% colnames()
full_data <- rbind(hbr_data, non_hbr_data)
    
## Add the risk computed using the quadratic risk model
full_data <- full_data %>%
    mutate(risk = estimate_risk(arc_hbr_score))

## Generate subsequent 12-month bleeding for each patient based
## on the bleeding risk (Bernoulli with p = bleeding risk)
full_data <- full_data %>%
    mutate(bleed = rbinom(n = n, size = 1, prob = p_hbr_criteria[["major_oac"]]))

saveRDS(full_data, "gendata/hbr_sim_dataset.R")

