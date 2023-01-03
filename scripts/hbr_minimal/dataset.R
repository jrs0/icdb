##' This file contains a script to obtain the dataset used for
##' minimal modelling of HBR from the primary diagnosis ICD field
##' of APC spell data. 

library(tidyverse)
library(lubridate)

## Run either devtools::load_all() or library(icdb), depending
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

## Parse (read and interpret) the primary diagnosis ICD code field.
## After this step, certain codes are kept (the ones the program knows
## how to interpret), and these are guaranteed to have been interpreted
## correctly. It rejects codes where it unsure of the meaning of the code.
code_file <- system.file("extdata/icd10/icd10_arc.yaml", package="icdb")
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

    ##     filter(diagnosis %in_group% c("acs", "bleeding"))


## Define the length of the post-index window
post <- ddays(365)

## Subtract one year from the end to get the date of
## the last valid index event (where there is one complete
## follow-up year).
index_end <- end - post



## 100,000 codes takes about 16 seconds currently (now 4 seconds, now 3s)
## 200,000 - 32 seconds (7 seconds, now 3s)
## 300,000 - 47 seconds (9 seconds, now 4s)
## 1,000,000 - not measured before (26 seconds, 7s)
## 6.5m - 15mins (2 minutes 24 seconds, 26s)

