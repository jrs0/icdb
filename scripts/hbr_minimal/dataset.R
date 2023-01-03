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
    count()

saveRDS(spells_of_interest, "gendata/spells_of_interest.rds")

## Save Point 1 ============================================
## If you get here, save the result so that you can pick up
## without needing to redo the parsing steps
spells_of_interest <- readRDS("gendata/spells_of_interest.rds")

## Group by nhs number and arrange by date to obtain the sequence
## of events for each patient. Label each acs condition with a
## unique id, and then fill forwards and backwards to label each
## other spell with the closest ACS event 
val <- spells_of_interest %>%
    group_by(nhs_number) %>%
    arrange(spell_start, .by_group = TRUE)

## Add an id to every row
a <- val %>% mutate(id = row_number())

## Pull out the ACS ids
b <- a %>% mutate(acs_id = case_when(grepl("acs", group) ~ id))

## Store the next and previous acs id for every other spell
c <- b %>% mutate(next_acs_id = acs_id) %>% fill(next_acs_id, .direction = "up") %>% mutate(prev_acs_id = acs_id) %>% fill(prev_acs_id, .direction = "down")

## Add a timestamp for the acs events
d <- c %>% mutate(acs_date = case_when(grepl("acs", group) ~ spell_start))

## Fill the date for the next/previous acs upwards and downwards
e <- d %>% mutate(next_acs_date = acs_date) %>% fill(next_acs_date, .direction = "up") %>% mutate(prev_acs_date = acs_date) %>% fill(prev_acs_date, .direction = "down")

## Drop groups not containing an acs event
f <- e %>% filter(any(grepl("acs", group)))

## Drop any groups 


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

