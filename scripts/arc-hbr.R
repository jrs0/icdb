##' This file contains attempts to calculate ARC-HBR
##' criteria from national APC datasets

library(tidyverse)
library(tidymodels)
library(lubridate)

## Remember that you have to rerun use_cache after
## a load_all()
use_cache(TRUE, size = 1, lifetime = ddays(30))

msrv <- mapped_server("xsw")

## Start and end dates for index percutaneous coronary intervention (PCI)
## procedures. This script will use acute coronary syndrome (ACS)
## diagnosis codes in lieu of procedure codes for now.
start <- ymd("2000-1-1")
end <- today()

## Define the length of the post-index window
post <- days(365)

## Subtract one year from the end to get the date of
## the last valid index event (where there is one complete
## follow-up year).
index_end <- end - post

## Fetch all spells between the dates (filter by the codes defined
## in the acs.yaml and bleeding.yaml files)
spells <- msrv$sus$apc_spells %>%
    filter(spell_start >= !!start, spell_end <= !!index_end) %>%
    codes_from(c("icd10/acs.yaml", "icd10/bleeding.yaml"), primary_diagnosis_icd) %>% 
    run()

## Get 
subsequent <- spells %>%
    ## Add a column "type" that contains either "acs" or "bleeding"
    mutate(type = drop_detail(primary_diagnosis_icd, 1)) %>% 
    ## Group by patient
    group_by(nhs_number) %>%
    ## Arrange in order of episode start and event type
    arrange(spell_start, type, .by_group = TRUE) %>%
    ## Only keep those groups which started with an ACS event
    filter(first(type) == "acs") %>%
    ## Only keep those groups that also contain a subsequent bleeding event
    filter(any(type == "bleeding"))# %>%
    ## Only keep groups where the bleeding even is within the post window
    ##filter(nth(spell_start, 2) < first(spell_start) - post)

