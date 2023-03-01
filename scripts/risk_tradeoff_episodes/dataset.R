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
##' - Renal (chronic kidney disease codes)
##' - Current smoker (using smoking ICD codes as proxy)
##'
##' Bleeding-specific risks
##' - Prior bleeding
##' - COPD
##' - Cancer
##' - Cirrhosis with portal hypertension (severe liver disease)
##' - Oral anticoagulant use (using atrial fibrillation as proxy)
##'
##' Ischaemia-specific risks
##' - Prior ischaemic stroke
##' - Diabetes treated with insulin/oral medication (using type-1 diabetes
##'   as proxy)
##' - STEMI presentation of ACS
##'
##' Index event
##' - Acute coronary syndromes (ACS)
##'
##' Response (death is currently excluded)
##' - Bleeding codes
##' - ACS (includes stent thrombosis?)
##' - Ischaemic stroke
##'
##' 
##' These are designed to map to the risk factors identified in "2021
##' Urban et al. - Assessing the Risks of Bleeding vs Thrombotic Events
##' in Patients at High Bleeding Risk After Coronary Stent Implantation
##' The ARC-High Bleeding Risk Trade-off Model". The mappings often do
##' not exactly map to the precise concept required, but some of the
##' grouping may correlate sufficiently to be useful. For example,
##' OAC are most commonly indicated by atrial fibrillation [2019 Urban],
##' but not all cases of AF may entail OAC use. Another example is that
##' use of insulin/oral medication may not be equivalent to having a
##' diabetes ICD code -- in this case, type-1 diabetes is used (often
##' treated with insulin), but this may not catch all cases of insulin/oral
##' medication use in patients with diabetes.
##' 
##' Spells are collected from the period 2000-1-1 to 2023-1-1. 
##' ICD codes are parsed from the primary diagnosis of these spells,
##' and grouped according to the categories defined in the response
##' variables. Only spells of interest are retained. All acs spells from
##' this collection are kept, apart from those where a full 12months
##' prior period and a full 12months follow up period is not available.
##' Predictors and response are calculated by analysing the window
##' surrounding the index acs event. The spell_start date is used as
##' the occurance date for all spells. Secondary diagnoses are ignored.
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

msrv <- mapped_server("xsw", mapping="mapping.yaml")

## Start and end dates for index percutaneous coronary intervention (PCI)
## procedures. This script will use acute coronary syndrome (ACS)
## diagnosis codes in lieu of procedure codes for now.
start <- ymd("2000-1-1")
end <- ymd("2023-1-1")

## Fetch all episodes between the dates (approx 10 million rows)
all_episodes <- msrv$sus$apc_episodes %>%
    filter(episode_start >= !!start, episode_start <= !!end) %>%
    run()
message("Total episodes: ", nrow(all_episodes))

## This is a workaround for now, because the cache destructor is not
## getting called correctly on package exit.
flush_level1()

## Note: before running the parts below, make sure the working
## directory is set to the location of this script

## Parse all the diagnoses fields.
code_file <- "icd10.yaml"
parsed_icd <- all_episodes %>%
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
    mutate(group = group_string(diagnosis)) %>%
    filter(group != "") %>%
    select(-diagnosis)
message("Total spells of interest (those in groups): ",
        nrow(spells_of_interest))

## Plot the distribution of different conditions (note the
## log scale). In viewing this graph, note that some conditions
## have a greater spells-per-person-per-time than others -- a
## acute condition may have one spell, but a chronic condition
## may have many.
spells_of_interest %>%
    count(group) %>%
    ggplot() +
    geom_bar(mapping = aes(x = reorder(group, -n), y = n), stat="identity") +
    scale_y_log10()

## Print a summary of the breakdown of each condition
spells_of_interest %>%
    count(group) %>%
    print(n=30)

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

## Reduce the ICD groups to the relevant groups of
## interest for the predictors and the response
with_id <- spells_of_interest %>%
    ## Add an id to every row that will become
    ## the id for index acs events. The data is
    ## arranged by nhs number and date so that
    ## grouping by id later will also perform this
    ## arrangement.
    arrange(nhs_number, spell_start) %>%
    mutate(id = row_number())

## Make the table of index acs events, and record whether
## the presentation was stemi or nstemi
index_acs <- with_id %>%
    filter(str_detect(group, "acs")) %>%
    mutate(stemi_presentation = str_detect(group, "acs_stemi")) %>%
    rename(age = age_on_admission,
           acs_date = spell_start,
           acs_id = id) %>%
    select(-group)
    
total_acs_events <- nrow(index_acs)
message("Total acs events: ", total_acs_events)

## Reduce the ICD groups to the relevant groups of
## interest for the predictors and the response, prior to
## joining the other spells and accounting for the number
## of occurances of each reduced group. It is important
## not to separate rows before finding the index events,
## in case any ACS events are double counted.
with_reduced_groups <- with_id %>%
    rename(other_spell_date = spell_start,
           other_spell_id = id) %>%
    ## Drop unnecessary columns
    select(-age_on_admission) %>%
    ## Seperate diagnosis rows that fall into two categories
    ## (e.g. anaemia,bleeding), into two rows, one for each
    ## group. This is so that they can be accounted for properly
    ## when the groups are accounted for in the pivot below.
    separate_rows(group, sep = ",") %>%
    ## Add predictor reduced groups
    mutate(predictor_group = case_when(
               ## Class any CKD as renal
               str_detect(group, "ckd") ~ "renal",
               ## Consider unstable angina as nstemi
               str_detect(group, "unstable_angina") ~ "acs_nstemi",
               ## Group cirrhosis, portal hypertension and hepatic failure
               str_detect(group, "(portal_hypertension|cirrhosis|hepatic_failure)") ~ "cirrhosis_portal_htn",
               TRUE ~ group)) %>%
    ## Drop excluded predictors
    filter(!str_detect(predictor_group, "(type_2_diabetes|diabetes_unspecified)")) %>%
    ## Create response column
    ## mutate(bleed_response = if_else(str_detect(group, "bleeding"), "bleeding", NA_character_)) %>%
    ## mutate(ischaemia_response = if_else(str_detect(group, "(acs|ischaemic_stroke)"), "ischaemia", NA_character_))
    ## Drop unnecessary columns
    select(-group)
           
## Print the reduced groups
with_reduced_groups %>%
    count(predictor_group) %>%
    print(n=30)

## Join back all the other spells onto the index
## events by nhs number
events_by_acs <- index_acs %>%
    left_join(with_reduced_groups, by=c("nhs_number"="nhs_number")) %>%
    ## Group this table by id.x, which is the index acs id
    group_by(acs_id) %>%
    arrange(acs_date, .by_group = TRUE)

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


## Want one column per ICD code containing the number of occurances
## of that code before the ACS event. 
with_code_columns <- events_in_window %>%
    ## Group by the other spell ID to count the number of occurances
    ## of that group in the previous 12 months
    group_by(predictor_group, .add=TRUE) %>%
    ## Count how many times each group occurs before the ACS.
    mutate(before = sum(other_spell_date < acs_date)) %>%
    ## Compute the response columns (whether a bleeding event or
    ## an ischaemic event occur after the ACS)
    mutate(bleed_after = sum((other_spell_date > acs_date) &
                             str_detect(predictor_group, "bleeding"))) %>%
    mutate(ischaemia_after = sum((other_spell_date > acs_date) &
                                 str_detect(predictor_group, "(acs|ischaemic_stroke)"))) %>%    
    ## Only keep one instance of each predictor group, because the
    ## count information is all we need.
    slice(1) %>%    
    ## In order to make the pivot wider step work later, the
    ## bleed_after count needs to be filled up and down in the acs
    ## group.
    group_by(acs_id) %>%
    mutate(bleed_after = max(bleed_after)) %>%
    mutate(ischaemia_after = max(ischaemia_after)) %>%
    ## Drop the other_spell_icd column so that the pivot wider works
    ## (i.e. the column values are unique)
    select(-other_spell_id, -other_spell_date) %>%
    ## Convert into a wide format where every predictor group becomes
    ## two columns of the form <code_name>_before and <code_name>_after,
    ## which store the number of times that diagnosis group occured before
    ## and after the ACS    
    pivot_wider(names_from = predictor_group,
                values_from = before,
                values_fill = list(before = 0),
                names_glue = "{predictor_group}_{.value}") %>%
    ungroup()


## Remove acs index events that do not have at least 12 months
## prior time, and do not have at least 12 months follow up time
pruned_dataset <- with_code_columns %>%
    filter(acs_date >= first_spell_date + window,
           acs_date <= last_spell_date - window)

risk_tradeoff_minimal_dataset <- pruned_dataset %>%
    rename(date = acs_date) %>%
    select(date, age, bleed_after, ischaemia_after, everything(), -nhs_number, -acs_id)

## Save the dataset
saveRDS(risk_tradeoff_minimal_dataset, "gendata/risk_tradeoff_minimal_dataset.rds")
