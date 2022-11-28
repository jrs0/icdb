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

## Get sequences of spells that began with an ACS event and contained
## at least one subsequent ACS or bleeding event.
subsequent <- spells %>%
    ## Add a column "type" that contains either "acs" or "bleeding"
    mutate(type = drop_detail(primary_diagnosis_icd, 1)) %>% 
    ## Group by patient
    group_by(nhs_number) %>%
    ## Arrange in order of spell start and event type
    arrange(spell_start, type, .by_group = TRUE) %>%
    ## Only keep those groups which started with an ACS event
    filter(first(type) == "acs") %>%
    ## Only keep those groups that also contain a subsequent bleeding event
    filter(any(type == "bleeding"))## %>%
    ## Only keep groups where the bleeding even is within the post window
    ##filter(nth(spell_start, 2) < first(spell_start) - post)

## Find ACS events that were followed by bleeding events
## within the post-index window. 
within_post <- subsequent %>%
    ## For each ACS event, calculate the time to the nearest
    ## subsequent bleeding event
    mutate(val = if_else(type == "bleeding", spell_start, NULL)) %>%
    fill(val, .direction = "up") %>%
    mutate(next_bleeding = val - spell_start)
