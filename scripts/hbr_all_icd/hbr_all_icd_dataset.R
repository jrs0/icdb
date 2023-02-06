3##' This file contains a script to obtain a dataset containing
##' the most commonly occuring prior ICD codes before a bleeding
##' event.
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
message("Total spells: ", nrow(all_spells))

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

## Plot the distribution of different ICD codes
ggplot(data=valid_icd) +
    geom_bar(mapping = aes(x = diagnosis), stat="count") +
    scale_y_log10()

## Distint diagnosis codes
distinct_codes <- valid_icd %>% count(diagnosis) %>% arrange(desc(n))
total_codes <- distinct_codes %>% nrow()
message("Total distinct diagnosis codes: ", total_codes)

## Find the coverage by keeping the most common codes
keep_proportion = 0.25
total_spells <- valid_icd %>% nrow()
kept_spells <- distinct_codes %>%
    head(keep_proportion * nrow(distinct_codes)) %>%
    pull(n) %>%
    sum()
message("Keeping ", keep_proportion, " of the distinct codes retains ", kept_spells/total_spells, " of spells")

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





