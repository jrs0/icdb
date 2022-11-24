##' This file contains attempts to calculate ARC-HBR
##' criteria from national APC datasets

library(tidyverse)
library(tidymodels)
library(lubridate)

use_cache(TRUE, size = 1)

msrv <- mapped_server("xsw")

## Start and end dates for index percutaneous coronary intervention (PCI)
## procedures. This script will use acute coronary syndrome (ACS)
## diagnosis codes in lieu of procedure codes for now.
start <- ymd("2000-1-1")
end <- today()

## Subtract one year from the end to get the date of
## the last valid index event (where there is one complete
## follow-up year).
index_end <- end - days(365)

## Fetch all spells between the dates (this is all ICD codes,
## even those not related to an ACS event)
spells <- msrv$sus$apc_spells %>%
    ##filter(spell_start >= !!start, spell_end <= !!index_end) %>%
    head(100) %>%
    run() %>%
    mutate(p = to_icd10(primary_diagnosis_icd))
