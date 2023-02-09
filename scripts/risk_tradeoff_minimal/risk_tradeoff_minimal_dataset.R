##' This file contains a script to obtain the dataset used for
##' minimal modelling of the high-bleeding-risk vs. ischaemic/thrombotic
##' risk trade-off.
##'
##' The output of this file is a dataset called hbr_minimal_dataset,
##' which contains the following columns:
##'
##' -- PREDICTORS --
##' date        - the date at which the index acute
##'               coronary syndrome (acs) occured
##' age         - the age of the patient at the time of acs (integer)
##' af          - whether the patient had an atrial fibrillation
##'               spell in the 12months prior to the acs
##'               ("prior_af" for yes, "none" for no)
##' ckd_n       - the highest stage chronic kidney disease (ckd) spell
##'               recorded for the patient in the last 12months ("none"
##'               for no ckd stage recorded. "stage_n" for prior stage n
##'               recorded.
##' ckd 
##' ckd_other   - whether various other ckd-related codes were seen
##'               in the last 12months before acs (see
##'               icd10_hbr_minimal.yaml for groups) ("prior_ckd" or
##'               "prior_ckd_other" for yes, "none" for no)
##' prior_bleed - was there a bleeding spell recorded in the previous
##'               12months ("prior_bleed" for yes, "none" for no)
##' acs         - Did an prior acs event occur in the previous 12months
##'               ("prior_acs" for yes, "none" for no)
##' others...
##'
##' 
##' -- RESPONSE --
##' bleed       - does a bleed occur in the period between the index
##'               acs event and the next acs event, or in period 12months
##'               after the index acs event if there is no subsequent acs
##'               event. "bleed_occured" for yes, "no_bleed" for no.
##' 
##' The dataset is created by looking at the primary diagnosis column in
##' HES spells data. The codes are collected into the following groups,
##' defined in the *icd10.yaml file.
##'
##' Bleeding/ischaemia risk factors:
##' - Anaemia (low haemoglobin)
##' - Thrombocytopenia (low platelet count)
##' - Renal (kidney function)
##' - Current smoker
##'
##' Bleeding-specific risks
##' - COPD
##' - Cancer
##' - Cirrhosis with portal hypertension (severe liver disease)
##' - Atrial fibrillation (proxy for oral anticoagulant use)
##'
##' Ischaemia-specific risks
##' - Diabetes
##' - STEMI presentation
##' 
##' 
##' These are designed to map to the risk factors identified in "2021
##' Urban et al. - Assessing the Risks of Bleeding vs Thrombotic Events
##' in Patients at High Bleeding Risk After Coronary Stent Implantation
##' The ARC-High Bleeding Risk Trade-off Model":
##'
##' 
##' 
##' 
##' 
##' Spells are collected from the period 2000-1-1 to 2023-1-1.
##' ICD codes are parsed from these spells, and grouped according to
##' the categories defined in the response variables, and only spells
##' of interest are retained. All acs spells from this collection are
##' retained in the dataset, apart from those where a full 12months
##' prior period and a full 12months follow up period is not available.
##' Predictors and response are calculated by analysing the window
##' surrounding the index acs event.
##' 
##' To run this script, make sure the working directory is set
##' to the location of this file. Ensure you have ICDB installed
##' (see the documentation for install instructions). You will need
##' to set up a database connection called "xsw" using the ODBC
##' data sources program. By default, mapped_server will use the
##' bnssg mapping files defined in inst/.
##'
##' WARNING: This script is currently untested. Use with caution.
##' 

library(tidyverse)
library(lubridate)
library(ggplot2)
library(icdb)

## Remember that you have to rerun use_cache after
## a load_all() or a library(icdb)
use_cache(TRUE, lifetime = ddays(30))

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
code_file <- "risk_tradeoff_minimal_icd10.yaml"
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

save_data <- list(spells_of_interest = spells_of_interest,
                  first_spell_date = first_spell_date,
                  last_spell_date = last_spell_date)                  
saveRDS(save_data, "gendata/spells_of_interest.rds")

## Save Point 1 ============================================
## If you get here, save the result so that you can pick up
## without needing to redo the parsing steps
save_data <- readRDS("gendata/spells_of_interest.rds")
spells_of_interest = save_data$spells_of_interest
first_spell_date = save_data$first_spell_date
last_spell_date = save_data$last_spell_date


## Create the dataset from the spells event list. The procedure is
## as follows:
##
## 1) Each acs event in the spells defines a row in the dataset.
##    The spells of interest are those in the 12months before that
##    event, called the prior period; and those in the 12months
##    after the event, or the time to the next acs event, whichever
##    is shorter (this is called the post period)
## 2) Each row has the following predictors: age, atrial
##    fibrillation (af), chronic kidney disease (ckd_n), ckd,
##    ckd_other, acs, and bleed. All but ckd_n are defined as
##    1 if that event occurs in the 12month prior to the acs,
##    and 0 otherwise. ckd_n is defined as the largest n that
##    occurs in the 12months prior period, or 0 otherwise.
## 3) Each row contains the response bleed, which is 1 if a
##    a bleed occurs in the post period, and 0 otherwise
##

## Add an id to every row that will become
## the id for index acs events. The data is
## arranged by nhs number and date so that
## grouping by id later will also perform this
## arrangement.
spells_of_interest <- spells_of_interest %>%
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
           other_spell_id = id.y,
           other_spell_group = group.y,
           other_spell_date = spell_start.y) %>%
    select(age, acs_id, acs_date, other_spell_id,
           other_spell_date, other_spell_group)

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

## Compute all the binary predictors (whether or not an event
## occured in the previous 12-months)
with_predictors <- events_in_window %>%
    mutate(af = if_else(any((other_spell_group == "af") &
                            (other_spell_date < acs_date)), 1, 0)) %>%
    mutate(ckd = if_else(any((other_spell_group == "ckd") &
                             (other_spell_date < acs_date)), 1, 0)) %>%
    mutate(ckd_other = if_else(any((other_spell_group == "ckd.other") &
                                   (other_spell_date < acs_date)), 1, 0)) %>%
    mutate(acs = if_else(any((other_spell_group == "acs") &
                             (other_spell_date < acs_date)), 1, 0)) %>%
    mutate(prior_bleed = if_else(any((other_spell_group == "bleeding") &
                                     (other_spell_date < acs_date)), 1, 0))
    
## Compute the numeric ckd_n score (the maximum ckd stage that
## occured in the previous 12-months). This is really slow and
## inelegant, 
with_ckd_n <- with_predictors %>% 
    mutate(ckd_n_tmp = case_when((other_spell_group == "ckd1") &
                                 (other_spell_date < acs_date) ~ 1,
                                 (other_spell_group == "ckd2") &
                                 (other_spell_date < acs_date) ~ 2,
                                 (other_spell_group == "ckd3") &
                                 (other_spell_date < acs_date) ~ 3,
                                 (other_spell_group == "ckd4") &
                                 (other_spell_date < acs_date) ~ 4,
                                 (other_spell_group == "ckd5") &
                                 (other_spell_date < acs_date) ~ 5,
                                 TRUE ~ 0)) %>%
    mutate(ckd_n = max(ckd_n_tmp)) %>%
    select(-ckd_n_tmp)
    
## Add the bleed response variable. The variable is 1 if a
## bleed occurs within 12 months of the index acs event,
## and also before the next acs event
dataset <- with_ckd_n %>%
    ## Only keep acs and bleeding spells -- all the other
    ## spells have already been used and are no longer needed
    filter(grepl("(acs|bleeding)", other_spell_group)) %>%
    ## Drop all prior spells, and keep only one subsequent spell
    filter(other_spell_date >= acs_date) %>%
    slice_min(n = 2, order_by = other_spell_date) %>%
    ## In each group (arranged by increasing date), mark the
    ## response (bleed) variable as 1 if there is a bleeding spell
    ## in the (two event) group consisting of the index acs spell
    ## and the next spell
    mutate(bleed = if_else(any(other_spell_group == "bleeding"), 1, 0)) %>%
    ## Only keep the index acs row (maybe this should be done by id)
    filter(other_spell_id == acs_id)

## Remove acs index events that do not have at least 12 months
## prior time, and do not have at least 12 months follow up time
pruned_dataset <- dataset %>%
    filter(acs_date >= first_spell_date + window,
           acs_date <= last_spell_date - window)

## Drop unnecessary columns
hbr_minimal_dataset <- pruned_dataset %>%
    ungroup() %>% 
    rename(date = acs_date) %>%
    select(date, age, af, ckd_n, ckd, ckd_other, prior_bleed, acs, bleed)

## Convert variables to factors (note that all the numerical values
## above are ordered, which fixes the order of these factors). This
## is very verbose and error prone, but there seems no way to convert
## directly from a 0/1 vector to an ordered factor (surprise!)
hbr_minimal_dataset$af <- factor(hbr_minimal_dataset$af)
levels(hbr_minimal_dataset$af) <- c("none", "prior_af")
hbr_minimal_dataset$ckd_n <- factor(hbr_minimal_dataset$ckd_n)
levels(hbr_minimal_dataset$ckd_n) <- c("none", "stage_1", "stage_2",
                                       "stage_3", "stage_4", "stage_5")
hbr_minimal_dataset$ckd_other <- factor(hbr_minimal_dataset$ckd_other)
levels(hbr_minimal_dataset$ckd_other) <- c("none", "prior_ckd_other")
hbr_minimal_dataset$ckd <- factor(hbr_minimal_dataset$ckd)
levels(hbr_minimal_dataset$ckd) <- c("none", "prior_ckd")
hbr_minimal_dataset$acs <- factor(hbr_minimal_dataset$acs)
levels(hbr_minimal_dataset$acs) <- c("none", "prior_acs")
hbr_minimal_dataset$prior_bleed <- factor(hbr_minimal_dataset$prior_bleed)
levels(hbr_minimal_dataset$prior_bleed) <- c("none", "prior_bleed")
hbr_minimal_dataset$bleed <- factor(hbr_minimal_dataset$bleed)
levels(hbr_minimal_dataset$bleed) <- c("no_bleed", "bleed_occured")

## Save the dataset
saveRDS(hbr_minimal_dataset, "gendata/hbr_minimal_dataset.rds")

## Save Point 2 ============================
hbr_minimal_dataset <- readRDS("gendata/hbr_minimal_dataset.rds")

## Brief summary and plots
summary(hbr_minimal_dataset)

## Distribution of age in the bleeding and non-bleeding
## groups (the biggest risk factor according to ARC-HBF)
ggplot(hbr_minimal_dataset, aes(x=age, fill=bleed)) +
geom_density(alpha = 0.3)

## Distribution of prior AF in the bleeding and non-bleeding
## groups (potentially a proxy for long-term oral anticoagulant
## therapy, the second biggest risk factor according to ARC-HBR). 
tab <- table(hbr_minimal_dataset$bleed, hbr_minimal_dataset$af)
message("Number of bleeds with and without prior AF:")
tab
message("Now with normalised proportions between bleed and non-bleed:")
prop.table(tab, margin = 2)

## Distribution of CKD stage in the bleeding and non-bleeding groups
## (the third most important risk factor according to ARC-HBR)
tab <- table(hbr_minimal_dataset$bleed, hbr_minimal_dataset$ckd_n)
message("Number of bleeds with stage of CKD:")
tab
message("Now with normalised proportions between bleed and non-bleed:")
prop.table(tab, margin=2)

## Distribution of the CKD (non-stage) codes 
tab <- table(hbr_minimal_dataset$bleed, hbr_minimal_dataset$ckd)
message("Number of bleeds with prior CKD code:")
tab
message("Now with normalised proportions between bleed and non-bleed:")
prop.table(tab, margin=2)

## Distribution of the other CKD (non-stage) codes 
tab <- table(hbr_minimal_dataset$bleed, hbr_minimal_dataset$ckd_other)
message("Number of bleeds with prior (other) CKD code:")
tab
message("Now with normalised proportions between bleed and non-bleed:")
prop.table(tab, margin=2)

## Distribution of prior bleeding in the bleeding and non-bleeding
## groups. Prior bleeding makes sense as a predictor of future bleeding.
tab <- table(hbr_minimal_dataset$bleed, hbr_minimal_dataset$prior_bleed)
message("Number of bleeds with prior bleed:")
tab
message("Now with normalised proportions between bleed and non-bleed:")
prop.table(tab, margin=2)

## Distribution of prior acs in the bleeding and non-bleeding
## groups.
tab <- table(hbr_minimal_dataset$bleed, hbr_minimal_dataset$acs)
message("Number of bleeds with prior acs:")
tab
message("Now with normalised proportions between bleed and non-bleed:")
prop.table(tab, margin=2)


