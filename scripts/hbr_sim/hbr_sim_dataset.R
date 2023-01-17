library(fabricatr)
library(GGally)
library(ggplot2)
library(dplyr)
library(purrr)

## Based on ARC-HBR validation paper (Cao et al.)

## Size of output / final dataset
n = 1e6

## Proportion of population at high bleeding risk (HBR)
p_hbr <- 0.444

n_hbr <- p_hbr * n
n_non_hbr <- n - n_hbr

## Proportion of the HBR population at different bleeding risks
p_arc_hbr = c("0"=0, "1"=0.616, "2"=0.291, "3"=0.079, "4"=0.014, "5"=0, "6"=0, "7"=0, "8"=0, "9"=0)

## Define the proportions of HBR patients to be in each HBR score category - central illustration figure
p_hbr_criteria = list(
    major_oac = 0.185,
    major_sev_ckd = 0.137,
    minor_mod_ckd = 0.396,
    major_sev_anaemia = 0.332,
    minor_mild_anaemia = 0.369,
    minor_bleed = 0.037,
    major_thrmcyt = 0.043,
    major_surgery = 0.082,
    major_cva = 0.207,
    major_malig = 0.168,
    minor_age = 0.468
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
    major_sev_anaemia = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["major_sev_anaemia"]]),
    major_oac = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["major_oac"]]),
    major_malig = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["major_malig"]]),
    major_sev_ckd = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["major_sev_ckd"]]),
    major_surgery = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["major_surgery"]]),
    major_thrmcyt = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["major_thrmcyt"]]),
    ## MIn_sampleOR criteria - from figure 1
    minor_age = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["minor_age"]]),
    minor_mod_ckd = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["minor_mod_ckd"]]),
    minor_mild_anaemia = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["minor_mild_anaemia"]]),
    major_cva = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["major_cva"]]),
    minor_bleed = rbinom(n = n_sample, size = 1, prob = p_hbr_criteria[["minor_bleed"]])) %>%
    ## Calculate the number of times the HBR category has been satisfied (1 for a major and 0.5 for minor)
    mutate(maj_score = rowSums(select(., starts_with("major"))),
           min_score = rowSums(select(., starts_with("minor"))) %/% 2,
           arc_hbr_score = map2_dbl(maj_score, min_score, sum)) %>%
    ## Ditch the scores we don't care about, i.e. the ones where we want 0% proportion, as defined int he p_hbr_criteria
    filter(between(arc_hbr_score,
                   left = as.numeric(names(p_arc_hbr)[first(which(p_arc_hbr != 0))]),
                   right = as.numeric(names(p_arc_hbr)[last(which(p_arc_hbr != 0))]))) %>%
    ## Apply the wanted proportions of each HBR score group
    group_by(arc_hbr_score) %>%
    group_modify(~ slice_head(.x, n = n_hbr * p_arc_hbr[[as.character(.y)]])) %>%
    ungroup()

## Insert zero rows to make up proportion of non-HBR
n_cols <- hbr_data %>% ncol()
mat <- matrix(integer(n_non_hbr * n_cols), nrow = n_non_hbr)
non_hbr_data <- as_tibble(mat)
names(non_hbr_data) <- hbr_data %>% colnames()
full_data <- rbind(hbr_data, non_hbr_data)

## Check the size of the HBR group
full_data %>%
    filter(arc_hbr_score > 0) %>%
    nrow() / nrow(full_data)
    
## Add the risk computed using the quadratic risk model
full_data <- full_data %>%
    mutate(risk = estimate_risk(arc_hbr_score))

saveRDS(full_data, "gendata/hbr_sim_dataset.R")

## Save point ===============

hbr_sim_dataset <- readRDS("gendata/hbr_sim_dataset.R")

hbr_dataset <- hbr_sim_dataset %>%
    filter(arc_hbr_score > 0)

## Plot the proportion of different HBR scores, compared
## to the target values in Cao et al.
hbr_score_prop <- hbr_dataset %>%
    mutate(n=n()) %>%
    group_by(arc_hbr_score) %>%
    summarise(prop = n()/first(n)) %>%
    mutate(arc_hbr_score = as.character(arc_hbr_score)) %>%
    left_join(data.frame(arc_hbr_score = names(config$arc_hbr_pct),
                         cao_prop = unname(config$arc_hbr_pct)),
              by = "arc_hbr_score") %>%
    pivot_longer(cols = c("prop","cao_prop"), names_to = "label", values_to = "value")
## Plot comparison
ggplot(data = hbr_score_prop) +
    geom_bar(mapping = aes(x = arc_hbr_score, y = value, fill = label), stat = "identity", position = "dodge") +
    theme_bw()

## Check the prevalence of each major/minor criterion in the HBR group
hbr_criteria_prop <- hbr_dataset %>%
    mutate(n=n()) %>%
    summarise(across(matches("major|minor"), ~ sum(.x)/first(n))) %>%
    pivot_longer(cols = colnames(.), names_to = "label", values_to = "value") %>%
    mutate(class = "prop")
hbr_criteria <- config[grepl("^pct", names(config))]
hbr_criteria_target <- tibble(label = paste0(names(hbr_criteria)),
                              value = unlist(hbr_criteria),
                              class = "target")
hbr_criteria_full <- rbind(hbr_criteria_prop, hbr_criteria_target)
## Plot the comparison
ggplot(data = hbr_criteria_full) +
    geom_bar(mapping = aes(x = label, y = value, fill = class), stat = "identity", position = "dodge") +
    theme_bw()
