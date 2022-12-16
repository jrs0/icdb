##' This file contains attempts to calculate ARC-HBR
##' criteria from national APC datasets

library(tidyverse)
library(tidymodels)
library(lubridate)
library(survival)
library(ggsurvfit)

## Remember that you have to rerun use_cache after
## a load_all()
use_cache(TRUE, size = 1, lifetime = ddays(30))

msrv <- mapped_server("xsw")

## Start and end dates for index percutaneous coronary intervention (PCI)
## procedures. This script will use acute coronary syndrome (ACS)
## diagnosis codes in lieu of procedure codes for now.
start <- ymd("2000-1-1")
end <- ymd("2022-12-5")

## Define the length of the post-index window
post <- ddays(365)

## Subtract one year from the end to get the date of
## the last valid index event (where there is one complete
## follow-up year).
index_end <- end - post

## Fetch all spells between the dates (filter by the codes defined
## in the acs.yaml and bleeding.yaml files)
all_spells <- msrv$sus$apc_spells %>%
    filter(spell_start >= !!start, spell_end <= !!index_end) %>%
    run()

## Path to codes file
code_file <- system.file("extdata/icd10/icd10_arc.yaml", package="icdb")

## 100,000 codes takes about 16 seconds currently (now 4 seconds, now 3s)
## 200,000 - 32 seconds (7 seconds, now 3s)
## 300,000 - 47 seconds (9 seconds, now 4s)
## 1,000,000 - not measured before (26 seconds, 7s)
## 6.5m - 15mins (2 minutes 24 seconds, 26s)
spells <- all_spells %>%
    mutate(diagnosis = icd10(primary_diagnosis_icd, code_file)) %>%
    filter(diagnosis %in_group% c("acs", "bleeding"))

## Get sequences of spells that began with an ACS event and contained
## at least one subsequent ACS or bleeding event. The result is
## a tibble grouped by patient, containing a chronological record
## of all ACS and bleeding events
subsequent <- spells %>%
    ## Group by patient
    group_by(nhs_number) %>%
    ## Arrange in order of spell start
    arrange(spell_start, .by_group = TRUE)

## Find ACS events that were followed by bleeding events
## within the post-index window. The result contains the most
## recent ACS event before a subsequent bleeding event which
## occured within less than the post-index window. This makes
## the assumption that the most recent ACS event before the
## bleeding event is the cause of the bleeding event.
next_bleed <- subsequent %>% head(1000) %>%
    ## For each bleeding event, calculate the time to the nearest
    ## (most recent) ACS event. The times
    ## to next bleeding are stored in the ACS rows (an NA is used
    ## if there is not subsequent bleeding event
    mutate(val = if_else(diagnosis %in_group% "bleeding", spell_start, NULL)) %>%
    fill(val, .direction = "up") %>%
    mutate(time_to_bleed = as.numeric((val - spell_start)/lubridate::ddays(1))) %>%
    ## In addition, store the bleeding diagnosis for the subsequent
    ## bleeding event.
    mutate(bleed_type = if_else(diagnosis %in_group% "bleeding", diagnosis, NULL)) %>%
    fill(bleed_type, .direction = "up") %>%
    ## Keep only the most recent ACS event before a bleeding event,
    ## and also ACS events with no subsequent bleeding event
    filter(diagnosis %in_group% "acs") %>%
    filter((time_to_bleed == min(time_to_bleed)) | is.na(time_to_bleed)) %>%
    ## Create a column for whether the row is right-censored or not.
    ## An NA in the time_to_bleed means that no bleeding event
    ## occured.
    mutate(status = case_when(is.na(time_to_bleed) ~ 0,
                              time_to_bleed > as.numeric(post/ddays(1)) ~ 0,
                              TRUE ~ 1)) %>%
    ## Set all the NA rows as at least post-index window time
    ## length. The NA means that no subsequent bleeding event
    ## occured, so this right censoring is a weaker statement
    mutate(time_to_bleed = replace_na(time_to_bleed, as.numeric(post/ddays(1))),
           time_to_bleed = if_else(time_to_bleed > as.numeric(post/ddays(1)),
                                   365,
                                   time_to_bleed)) %>%
    ## Clean up by dropping temporary columns and renaming
    ungroup() %>%
    select(-val, -primary_diagnosis_icd, -spell_end, -nhs_number) %>%
    rename(acs_diagnosis = diagnosis, age = age_on_admission) %>%
    relocate(age, spell_start, acs_diagnosis,
             status, time_to_bleed, bleed_type)

## Age is a minor ARC-HBR criterion: score = 0.5 if age >= 75,
## else score is zero (higher means more at risk)
hbr <- next_bleed %>%
    mutate(hbr_age = case_when(age >= 75 ~ 0.5, TRUE ~ 0))

## Use survival analysis to model the likelihood of bleeding
## within 12-months of an ACS event, based on age
survfit2(Surv(time_to_bleed, status) ~ factor(hbr_age, labels = c("Under 75", "75+")), data = hbr) %>% 
    ggsurvfit() +
    labs(x = "Days",
         y = "Overall survival probability") +
    add_confidence_interval() +
    scale_x_continuous() +
    scale_y_continuous(labels = scales::percent) +
    scale_colour_discrete("Age flag")
