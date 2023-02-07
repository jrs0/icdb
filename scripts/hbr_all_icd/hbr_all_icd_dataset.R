##' This file contains a script to obtain a dataset containing
##' the most commonly occuring prior ICD codes before a bleeding
##' event.
##'
##' The output of this file is a dataset called hbr_all_icd_dataset,
##' which contains the following columns:
##'
##' -- PREDICTORS --
##' date           - the date at which the index acute
##'                  coronary syndrome (acs) occured
##' age            - the age of the patient at the time of acs (integer)
##' <code>_before  - how many of the ICD code <code> occured in the 12
##'                  months before the acs event
##'
##' -- RESPONSE --
##' bleed_after    - how many bleeding events occur in the 12 months
##'                  following the ACS event (integer). Bleeding codes
##'                  included are defined in the icd10.yaml codes file.
##' 
##' -- ADDITIONAL --
##' <code>_after   - how many of the ICD code <code> occured in the 12
##'                  months after the acs event.
##' 
##' Spells are collected from the period 2000-1-1 to 2023-1-1.
##' ICD codes are parsed from these spells, and only codes in the
##' +-12 months around an acs are retained. The number of occurances
##' of each type of code is counted. Rows are only kept if the acs has
##' a full +- 12 months window in the dataset.
##' 
##' To run this script, make sure the working directory is set
##' to the location of this file. Ensure you have ICDB installed
##' (see the documentation for install instructions). You will need
##' to set up a database connection called "xsw" using the ODBC
##' data sources program. By default, mapped_server will use the
##' bnssg mapping files defined in inst/.
##'

library(tidyverse)
library(lubridate)
library(ggplot2)
library(icdb)

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
message("Total spells: ", nrow(all_spells))icd_exampleicd_example

## Note: before running the parts below, make sure the working
## directory is set to the location of this script

## Parse (read and interpret) the primary diagnosis ICD code field.
## After this step, certain codes are kept (the ones the program knows
## how to interpret), and these are guaranteed to have been interpreted
## correctly. It rejects codes where it unsure of the meaning of the code.
## Note the codes file groupings are not used in this script, but the
## codes file must still be present
code_file <- "icd10.yaml"
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
## diagnosis column. Conversion to character vectors is very slow!
valid_icd <- parsed_icd %>%
    filter(is_valid(diagnosis)) %>% 
    select(-primary_diagnosis_icd, -spell_end) %>%
    mutate(group = group_string(diagnosis)) %>%
    mutate(diagnosis = as.character(diagnosis))

saveRDS(valid_icd, "gendata/valid_icd.rds")

## Save point 1 ==============================================
valid_icd <- readRDS("gendata/valid_icd.rds")

## Get the data range covered by the spells -- this is the range
## for which it is assumed data is present
first_spell_date <- min(valid_icd$spell_start)
last_spell_date <- max(valid_icd$spell_start)

## Distint diagnosis codes
distinct_codes <- valid_icd %>% count(diagnosis) %>% arrange(desc(n))
total_codes <- distinct_codes %>% nrow()
message("Total distinct diagnosis codes: ", total_codes)

## Find the coverage by keeping the most common codes
keep_proportion = 0.10
total_spells <- valid_icd %>% nrow()
kept_spells <- distinct_codes %>%
    head(keep_proportion * nrow(distinct_codes)) %>%
    pull(n) %>%
    sum()
message("Keeping ", keep_proportion, " of the distinct codes retains ", kept_spells/total_spells, " of spells")
message("Total codes reduced from ", total_codes, " to ", keep_proportion * total_codes)

## Add an id to every row that will become
## the id for index acs events. The data is
## arranged by nhs number and date so that
## grouping by id later will also perform this
## arrangement.
spells_of_interest <- valid_icd %>%
    arrange(nhs_number, spell_start) %>%
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
    arrange(spell_start.x, .by_group = TRUE) %>%
    ## Note: do not remove the duplicated acs index event row,
    ## because this will remove acs events with no prior and post
    ## events. Perform some cleanup of column names here. Note that
    ## each column represents an index acs event
    rename(age = age_on_admission.x,
           acs_id = id.x,
           acs_date = spell_start.x,
           acs_diagnosis = diagnosis.x,
           other_spell_id = id.y,
           other_spell_group = group.y,
           other_spell_diagnosis = diagnosis.y,
           other_spell_date = spell_start.y) %>%
    select(age, acs_id, acs_diagnosis, acs_date, other_spell_id,
           other_spell_date, other_spell_diagnosis, other_spell_group)

## Keep only spells which are within 12 months of the index
## acs event (before or after)
window <- ddays(365)
events_in_window <- events_by_acs %>%
    filter(other_spell_date >= acs_date - window,
           other_spell_date <= acs_date + window)

## Total number of different ICD codes after filtering +-12months around
## an ACS event.
total_codes <- events_in_window %>%
    ungroup() %>%
    count(other_spell_diagnosis) %>%
    nrow()
message("Total distinct diagnosis codes reduced to ", total_codes, " around ACS events")

## Count the number of acs events without any other spells in +- 12 months 
total_isolated_acs <- events_in_window %>%
    count(acs_id) %>%
    filter(n == 1) %>%
    nrow()
total_isolated_proportion <- total_isolated_acs / total_acs_events
message("ACS events with no other spell in +- 12 months: ",
        total_isolated_acs,
        " (", round(100*total_isolated_proportion, 2), "%)")

## Want one column per ICD code containing the number of occurances
## of that code before the ACS event. 
with_code_columns <- events_in_window %>% head(1000) %>%
    ## Group by the other spell ID to count the number of occurances
    ## of that group in the previous 12 months
    group_by(other_spell_diagnosis, .add=TRUE) %>%
    ## Count how many times each diagnosis code occurs before the ACS.
    mutate(before = sum(other_spell_date < acs_date )) %>%
    ## Do the same for all subsequent spells
    mutate(after = sum(other_spell_date >= acs_date )) %>%
    ## Also keep a specific flag to indicate whether a subsequent
    ## bleed occured after the ACS
    mutate(bleed_after = sum((other_spell_date >= acs_date) &
                             (other_spell_group == "bleeding"))) %>%
    ## Only keep one instance of each diagnosis code, because the
    ## count information is all we need.
    slice(1) %>%
    ## Filter out the ACS row itself
    filter(acs_id != other_spell_id) %>%
    ## Drop the other_spell_icd column so that the pivot wider works
    ## (i.e. the column values are unique)
    select(-other_spell_id, -other_spell_date)

    ## Convert into a wide format where every diagnosis code becomes
    ## two columns of the form <code_name>_before and <code_name>_after,
    ## which store the number of times that diagnosis occured before
    ## and after the ACS    
    pivot_wider(names_from = other_spell_diagnosis,
                values_from = c(before, after),
                values_fill = list(before = 0, after = 0),
                names_glue = "{other_spell_diagnosis}_{.value}")

## Remove acs index events that do not have at least 12 months
## prior time, and do not have at least 12 months follow up time
pruned_dataset <- with_code_columns %>%
    ungroup() %>%
    filter(acs_date >= first_spell_date + window,
           acs_date <= last_spell_date - window)

## Prepare the final dataset
hbr_all_icd_dataset <- pruned_dataset %>%
    rename(date = acs_date) %>%
    select(date, age, matches("(_before|_after)$"))

saveRDS(hbr_all_icd_dataset, "gendata/hbr_all_icd_dataset.rds")
