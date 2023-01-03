##' This file contains a script to obtain the dataset used for
##' minimal modelling of HBR from the primary diagnosis ICD field
##' of APC spell data. 

library(tidyverse)
library(lubridate)

## Remember that you have to rerun use_cache after
## a load_all()
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

## Parse (read and interpret) the primary diagnosis ICD code field.
## After this step, certain codes are kept (the ones the program knows
## how to interpret), and these are guaranteed to have been interpreted
## correctly. It rejects codes where it unsure of the meaning of the code.
code_file <- system.file("extdata/icd10/icd10_arc.yaml", package="icdb")
spells <- all_spells %>%
    mutate(diagnosis = icd10(primary_diagnosis_icd, code_file))

## Print basic statistics on the parsing results

## %>%
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

