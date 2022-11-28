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
## at least one subsequent ACS or bleeding event. The result is
## a tibble grouped by patient, containing a chronological record
## of all ACS and bleeding events following an ACS event. This makes
## the assumption that bleeding events are only of interest after
## at least one ACS event has occured.
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
    filter(any(type == "bleeding"))

## Find ACS events that were followed by bleeding events
## within the post-index window. The result contains the most
## recent ACS event before a subsequent bleeding event which
## occured within less than the post-index window. This makes
## the assumption that the most recent ACS event is the cause
## of the bleeding event.
next_bleed <- subsequent %>%
    ## For each bleeding event, calculate the time to the nearest
    ## (most recent) ACS event. This is the 1-backwards strategy,
    ## where an event B is assumed to be caused by the most recent
    ## event A (fails if multiple As really cause B). The times
    ## to next bleeding are stored in the ACS rows (an NA is used
    ## if there is not subsequent bleeding event
    mutate(val = if_else(type == "bleeding", spell_start, NULL)) %>%
    fill(val, .direction = "up") %>%
    mutate(time_to_bleed = val - spell_start) %>%
    ## Keep only the most recent ACS event before a bleeding event,
    ## and also ACS events with no subsequent bleeding event
    filter(type == "acs") %>%
    filter(time_to_bleed == min(time_to_bleed) | is.na(time_to_bleed)) %>%
    ## Create a column for whether a bleed occured in the post-index
    ## window
    mutate(post_bleed = case_when(time_to_bleed < post ~ TRUE, TRUE ~ FALSE))


    ## Only keep groups that have a subsequent bleeding event
    ## within the post-index window
    filter(next_bleeding < post) %>%
    ## Only keep the ACS event that has the smallest time
    ## to next bleeding event, i.e., the most recent ACS
    ## event before the bleeding event.
    filter(type == "acs") %>%
    filter(next_bleeding == min(next_bleeding))
