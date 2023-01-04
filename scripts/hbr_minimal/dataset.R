##' This file contains a script to obtain the dataset used for
##' minimal modelling of HBR from the primary diagnosis ICD field
##' of APC spell data. 

library(tidyverse)
library(lubridate)
library(ggplot2)

## To run this script, make sure the working directory is set
## to the location of this file.

## Run either devtools::load_all("../../") or library(icdb), depending
## whether you are developing the package or have an installed version

## Remember that you have to rerun use_cache after
## a load_all() or a library(icdb)
use_cache(TRUE, size = 1, lifetime = ddays(30))

msrv <- mapped_server("xsw")

## Start and end dates for index percutaneous coronary intervention (PCI)
## procedures. This script will use acute coronary syndrome (ACS)
## diagnosis codes in lieu of procedure codes for now.
start <- ymd("2000-1-1")
end <- ymd("2023-1-1")

## Fetch all spells between the dates (approx 6.8 million rows)
all_spells <- msrv$sus$apc_spells %>%
    filter(spell_start >= !!start, spell_start <= !!end) %>%
    run()
message("Total spells: ", nrow(all_spells))

## Note: before running the parts below, make sure the working
## directory is set to the location of this script

## Parse (read and interpret) the primary diagnosis ICD code field.
## After this step, certain codes are kept (the ones the program knows
## how to interpret), and these are guaranteed to have been interpreted
## correctly. It rejects codes where it unsure of the meaning of the code.
code_file <- "icd10_hbr_minimal.yaml"
parsed_icd <- all_spells %>%
    mutate(diagnosis = icd10(primary_diagnosis_icd, code_file))
parse_stats <- parsed_icd$diagnosis %>% get_parse_stats()
total_valid <- parse_stats$valid_count + parse_stats$trailing_count
total_excluded <- parse_stats$empty_count + parse_stats$invalid_count
message("Total valid ICD codes: ", total_valid)
message("Total invalid/non-interpreted ICD codes: ", total_excluded)
message("Percentage excluded: ",
        round(100*total_excluded/nrow(all_spells), 2), "%")

## Keep only the ICD codes that were parse correctly (with optional
## trailing matter). Reject empty/invalid codes. Drop the original
## diagnosis column.
valid_icd <- parsed_icd %>%
    filter(is_valid(diagnosis)) %>% 
    select(-primary_diagnosis_icd, -spell_end)

## Get the data range covered by the spells -- this is the range
## for which it is assumed data is present
first_spell_date <- min(valid_icd$spell_start)
last_spell_date <- max(valid_icd$spell_start)

## Filter the valid ICD codes only keeping the ones in a specified
## ICD-10 group (set by using the map-editor tool). In addition,
## extract the groups as strings and drop the original diagnosis
## column.
spells_of_interest <- valid_icd %>%
    filter(in_any_group(diagnosis)) %>% 
    mutate(group = group_string(diagnosis)) %>%
    select(-diagnosis)
message("Total spells of interest (those in groups): ",
        nrow(spells_of_interest))

## Plot the distribution of different conditions (note the
## log scale). In viewing this graph, note that some conditions
## have a greater spells-per-person-per-time than others -- a
## acute condition may have one spell, but a chronic condition
## may have many.
ggplot(data=spells_of_interest) +
    geom_bar(mapping = aes(x = group), stat="count") +
    scale_y_log10()

## Print a summary of the breakdown of each condition
spells_of_interest %>%
    select(group) %>%
    group_by(group) %>%
    count() %>%
    ungroup()

saveRDS(spells_of_interest, "gendata/spells_of_interest.rds")

## Save Point 1 ============================================
## If you get here, save the result so that you can pick up
## without needing to redo the parsing steps
spells_of_interest <- readRDS("gendata/spells_of_interest.rds")

## Create the dataset from the spells event list. The procedure is
## as follows:
##
## 1) Each acs event in the spells defines a row in the dataset.
##    The spells of interest are those in the 12months before that
##    event, called the prior period; and those in the 12months
##    after the event, or the time to the next acs event, whichever
##    is shorter (this is called the post period)
## 2) Each row has the following predictors: age, atrial
##    fibrillation (af), chronic kidney disease (ckd.n), ckd,
##    ckd.other, acs, and bleed. All but ckd.n are defined as
##    1 if that event occurs in the 12month prior to the acs,
##    and 0 otherwise. ckd.n is defined as the largest n that
##    occurs in the 12months prior period, or 0 otherwise.
## 3) Each row contains the response bleed, which is 1 if a
##    a bleed occurs in the post period, and 0 otherwise
##

## Add an id to every row that will become
## the id for index acs events
spells_of_interest <- spells_of_interest %>%
    mutate(id = row_number())

## Make the table of index acs events
index_acs <- spells_of_interest %>%
    filter(grepl("acs", group))

total_acs_events <- nrow(index_acs)
message("Total acs events: ", total_acs_events)

## Join back all the other spells onto the index
## events by nhs number
events_by_acs <- index_acs %>%
    left_join(spells_of_interest, by=c("nhs_number"="nhs_number")) %>%
    ## Group this table by id.x, which is the index acs id
    group_by(id.x) %>%
    ## Note: do not remove the duplicated acs index event row,
    ## because this will remove acs events with no prior and post
    ## events. Perform some cleanup of column names here. Note that
    ## each column represents an index acs event
    rename(acs_id = id.x,
           age = age_on_admission.x,
           other_spell_group = group.y,
           acs_date = spell_start.x,
           other_spell_date = spell_start.y) %>%
    select(acs_id, age, acs_date, other_spell_date, other_spell_group)

## Keep only spells which are within 12 months of the index
## acs event (before or after)
window <- ddays(365)
events_in_window <- events_by_acs %>%
    filter(other_spell_date >= acs_date - window,
           other_spell_date <= acs_date + window)

## Count the number of acs events without any other spells in +- 12 months 
total_isolated_acs <- events_in_window %>%
    count(acs_id) %>%
    filter(n == 1) %>%
    nrow()
total_isolated_proportion <- total_isolated_acs / total_acs_events
message("ACS events with no other spell in +- 12 months: ",
        total_isolated_acs,
        " (", round(100*total_isolated_proportion, 2), "%)")

## Create new columns for the predictors (spells in the prior
## 12 months
with_predictors <- events_in_window %>%
    mutate(af = if_else(any((other_spell_group == "af") &
                            (other_spell_date < acs_date)), 1, 0)) %>%
    mutate(ckd = if_else(any((other_spell_group == "ckd") &
                             (other_spell_date < acs_date)), 1, 0)) %>%
    mutate(ckd.other = if_else(any((other_spell_group == "ckd.other") &
                                   (other_spell_date < acs_date)), 1, 0)) %>%
    mutate(ckd.n = if_else(any((other_spell_group == "ckd.other") &
                               (other_spell_date < acs_date)), 1, 0))
    


    
val <- spells_of_interest %>%
    ## Add an id to every row
    mutate(id = row_number()) %>%
    ## Group by patient
    group_by(nhs_number) %>%
    arrange(spell_start, .by_group = TRUE) %>%
    ## Drop groups not containing an acs event
    filter(any(grepl("acs", group)))


a <- val %>%
    left_join(spells_of_interest, by=c("nhs_number"="nhs_number"))


## Begin building up the data set rows, starting with the acs
## event (the index events)
prior <- val %>%
    filter(grepl("acs", group)) %>%
    left_join(val, by=c("id"="next_acs_id"))
    ## Drop any spells that occur more than 12months
    ## before the index acs event
    ##filter(spell_start.y >= spell_start.x - ddays(365))
    ## Calculate new predictors from the 

## Define the length of the post-index window

## Subtract one year from the end to get the date of
## the last valid index event (where there is one complete
## follow-up year).
index_end <- end - post



## 100,000 codes takes about 16 seconds currently (now 4 seconds, now 3s)
## 200,000 - 32 seconds (7 seconds, now 3s)
## 300,000 - 47 seconds (9 seconds, now 4s)
## 1,000,000 - not measured before (26 seconds, 7s)
## 6.5m - 15mins (2 minutes 24 seconds, 26s)

