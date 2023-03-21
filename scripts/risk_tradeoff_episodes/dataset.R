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
start <- ymd("2020-1-1")
end <- ymd("2023-1-1")

## Fetch all episodes between the dates (approx 10 million rows)
all_episodes <- msrv$sus$apc_episodes %>%
    filter(episode_start >= !!start, episode_start <= !!end) %>%
    run()
message("Total episodes: ", nrow(all_episodes))

## Fetch all the mortality between the two dates
all_mortality <- msrv$mort$civ_reg %>%
    filter(date_of_death >= !!start, date_of_death <= !!end) %>%
    run()
    
## This is a workaround for now, because the cache destructor is not
## getting called correctly on package exit.
flush_level1()

## Note: before running the parts below, make sure the working
## directory is set to the location of this script

## Parse all the diagnoses fields.
code_file <- "icd10.yaml"

#####################################################################
## Starting here, you are on thin ice memory wise -- the icd10 class
## uses a very large amount of memory

## This takes about 12 minutes unparallelised
parsed_icd <- all_episodes %>%
    mutate(across(matches("diagnosis"), ~ icd10(.x, code_file)))

parsed_mort <- all_mortality %>%
    mutate(across(matches("icd"), ~ icd10(.x, code_file)))

## Keep only the ICD codes where the primary diagnosis is valid. This
## uses base R to avoid any memory copies of any icd10 columns
parsed_icd <- parsed_icd[is_valid(primary_diagnosis_icd),]
gc()
gc()

## Now drop all the ICD columns in place. Being very close to the
## memory limit (64 GiB), this one took ages, but then running the
## gc frees up some space.
parsed_icd$primary_diagnosis_icd <- group_string(parsed_icd$primary_diagnosis_icd)
gc() ## Needed running twice the first time to get itself sorted
gc()

parsed_icd$secondary_diagnosis_1_icd <- group_string(parsed_icd$secondary_diagnosis_1_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_2_icd <- group_string(parsed_icd$secondary_diagnosis_2_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_3_icd <- group_string(parsed_icd$secondary_diagnosis_3_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_4_icd <- group_string(parsed_icd$secondary_diagnosis_4_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_5_icd <- group_string(parsed_icd$secondary_diagnosis_5_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_6_icd <- group_string(parsed_icd$secondary_diagnosis_6_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_7_icd <- group_string(parsed_icd$secondary_diagnosis_7_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_8_icd <- group_string(parsed_icd$secondary_diagnosis_8_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_9_icd <- group_string(parsed_icd$secondary_diagnosis_9_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_10_icd <- group_string(parsed_icd$secondary_diagnosis_10_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_11_icd <- group_string(parsed_icd$secondary_diagnosis_11_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_12_icd <- group_string(parsed_icd$secondary_diagnosis_12_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_13_icd <- group_string(parsed_icd$secondary_diagnosis_13_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_14_icd <- group_string(parsed_icd$secondary_diagnosis_14_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_15_icd <- group_string(parsed_icd$secondary_diagnosis_15_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_16_icd <- group_string(parsed_icd$secondary_diagnosis_16_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_17_icd <- group_string(parsed_icd$secondary_diagnosis_17_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_18_icd <- group_string(parsed_icd$secondary_diagnosis_18_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_19_icd <- group_string(parsed_icd$secondary_diagnosis_19_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_20_icd <- group_string(parsed_icd$secondary_diagnosis_20_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_21_icd <- group_string(parsed_icd$secondary_diagnosis_21_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_22_icd <- group_string(parsed_icd$secondary_diagnosis_22_icd)
gc()
gc()

parsed_icd$secondary_diagnosis_23_icd <- group_string(parsed_icd$secondary_diagnosis_23_icd)
gc()
gc()

saveRDS(parsed_icd, "gendata/parsed_icd_char.rds")

## End of the silly memory issues ############################################

## 1 minute
parsed_icd <- readRDS("gendata/parsed_icd_char.rds")

## Keep less data (R script too slow)
start <- ymd("2015-1-1")
end <- ymd("2023-1-1")
parsed_icd <- parsed_icd %>%
    filter(episode_start >= start, episode_end <= end)

## Get the data range covered by the spells -- this is the range
## for which it is assumed data is present
first_episode_date <- min(parsed_icd$episode_start)
last_episode_date <- max(parsed_icd$episode_start)

## Reduce the ICD groups to the relevant groups of
## interest for the predictors and the response
## (several minutes)
with_id <- parsed_icd %>%
    ## Add an id to every row that will become
    ## the id for index acs events. The data is
    ## arranged by nhs number and date so that
     ## grouping by id later will also perform this
    ## arrangement.
    arrange(nhs_number, episode_start) %>%
    mutate(id = row_number())

## Make the table of index acs events, and record whether
## the presentation was stemi or nstemi
## (about 10 mins)
index_acs <- with_id %>%
    ## You want to group by spell ID here, but you can't because
    ## some spell IDs are empty. So filter out all the rows with
    ## empty spell ID
    filter(hospital_provider_spell_identifier != "") %>%
    ## Collect episodes into spells
    group_by(hospital_provider_spell_identifier) %>%
    ## Arrange the episodes by start date and then end date. For equal
    ## start dates, if the episode ends sooner it is assumed to be before
    arrange(episode_start, episode_end) %>%
    ## Keep only the groups (spells) which begin with an ACS primary episode
    filter(str_detect(first(primary_diagnosis_icd), "acs")) %>%
    ## Get only that ACS episode row 
    slice_head(n = 1) %>%
    mutate(stemi_presentation = str_detect(primary_diagnosis_icd, "acs_stemi")) %>%
    rename(age = age_on_admission,
           acs_date = episode_start,
           acs_id = id) %>%
    ## Drop all the diagnoses columns -- the goal is to have no duplicate columns
    ## names for the left join that comes next
    select(-matches("diagnosis")) %>%
    ## Drop the episode end date
    select(-episode_end) %>%
    ungroup()

total_acs_events <- nrow(index_acs)
message("Total acs events: ", total_acs_events)

## Reduce the ICD groups to the relevant groups of
## interest for the predictors and the response, prior to
## joining the other spells and accounting for the number
## of occurances of each reduced group. It is important
## not to separate rows before finding the index events,
## in case any ACS events are double counted. After this,
## you will get a dataframe with a "predictor_group" which
## contains all the diagnosis information (from all the
## primary and secondary columns), and a diagnosis_type
## column which lists which primary/secondary column it
## came from (i.e. the data is long). Any empty diagnoses
## are then dropped.
with_reduced_groups <- with_id %>%
    ## Rename episode date and episode ID so that they will
    ## get a good name on the left join with acs index events
    rename(other_episode_date = episode_start,
           other_episode_id = id) %>%
    ## Drop unnecessary columns
    select(-age_on_admission, -episode_end) %>%
    ## Seperate diagnosis rows that fall into two categories
    ## (e.g. anaemia,bleeding), into two rows, one for each
    ## group. This is so that they can be accounted for properly
    ## when the groups are counted in the pivot below.
    separate_rows(matches("diagnosis"), sep = ",") %>%
    ## In all primary and secondary diagnosis columns, group together the ICD groups
    ## from the mapping file into groups of interest for predictors. This creates a
    ## slightly coarser-grained grouping than the mapping file (the advantage of doing
    ## this here is that you don't need to refetch from the database to change the groups)
    mutate(across(matches("diagnosis"), ~ case_when(
                                           ## Class any CKD as renal
                                           str_detect(.x, "ckd") ~ "renal",
                                           ## Consider unstable angina as nstemi
                                           str_detect(.x, "unstable_angina") ~ "acs_nstemi",
                                           ## Group cirrhosis, portal hypertension and hepatic failure
                                           str_detect(.x, "(portal_hypertension|cirrhosis|hepatic_failure)") ~ "cirrhosis_portal_htn",
                                           ## Group together all diabetes
                                           str_detect(.x, "(type_1_diabetes|type_2_diabetes|diabetes_unspecified)") ~ "diabetes",
                                           TRUE ~ .x))) %>%
    ## Collapse all the diagnoses (primary and secondary) into one big "predictor_group" column. Then remove any rows that do not
    ## contain a diagnosis (these do not contribute information).
    pivot_longer(matches("diagnosis"), names_to = "diagnosis_type", values_to = "predictor_group") %>%
    filter(predictor_group != "") %>%
    ## Create response column from the primary diagnosis. 
    ## mutate(bleed_response = if_else(diagnosis_type == "primary_diagnosis_icd" & predictor_group == "bleeding",
    ##                                 "bleeding", NA_character_)) %>%
    ## mutate(ischaemia_response = if_else(diagnosis_type == "primary_diagnosis_icd" & str_detect(predictor_group, "(acs|ischaemic_stroke)"),
    ##                                     "ischaemia", NA_character_)) %>%
    ## We don't need anything except the NHS number, the dates, and the diagnoses
    select(nhs_number, matches("diagnosis"), predictor_group, matches("episode"), matches("response"))

## Join back all the other spells onto the index
## events by nhs number
events_by_acs <- index_acs %>%
    left_join(with_reduced_groups, by=c("nhs_number"), multiple = "all") %>%
    ## Group this table by id.x, which is the index acs id
    group_by(acs_id) %>%
    arrange(acs_date, .by_group = TRUE)

## Keep only spells which are within 12 months of the index
## acs event (before or after)
window <- ddays(365)
events_in_window <- events_by_acs %>%
    filter(other_episode_date >= acs_date - window,
           other_episode_date <= acs_date + window)

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
    mutate(before = sum(other_episode_date < acs_date)) %>%
    ## Compute the response columns (whether a bleeding event or
    ## an ischaemic event occur after the ACS)
    mutate(bleed_after = sum((other_episode_date > acs_date) &
                             str_detect(predictor_group, "bleeding"))) %>%
    mutate(ischaemia_after = sum((other_episode_date > acs_date) &
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
    ## Deduplicate the ACS index row (only keep the primary episode or
    ## the index spell)
    filter(!((acs_id == other_episode_id) & (diagnosis_type != "primary_diagnosis_icd"))) %>%
    ## Drop the other_spell_icd column so that the pivot wider works
    ## (i.e. the column values are unique)
    select(-other_episode_id, -other_episode_date, -diagnosis_type) %>%
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
    filter(acs_date >= first_episode_date + window,
           acs_date <= last_episode_date - window)

risk_tradeoff_episodes_dataset <- pruned_dataset %>%
    rename(date = acs_date) %>%
    select(date, age, bleed_after, ischaemia_after, everything(), -nhs_number, -acs_id, -hospital_provider_spell_identifier)

## Save the dataset
saveRDS(risk_tradeoff_episodes_dataset, "gendata/dataset_2015_2023.rds")
