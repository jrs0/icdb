## I haven't checked this, may need to double check path etc.
code_file <- system.fil("extdata/icd10/icd10_example.yaml", package="icdb")
parsed_icd <- all_spells %>%
    mutate(diagnosis = icd10(primary_diagnosis_icd, code_file))

## Keep only the ICD codes that were parse correctly (with optional
## trailing matter). Reject empty/invalid codes. Drop the original
## diagnosis column.
valid_icd <- parsed_icd %>%
    filter(is_valid(diagnosis)) %>% 
    select(-primary_diagnosis_icd, -spell_end)

## Filter the valid ICD codes only keeping the ones in a specified
## ICD-10 group (set by using the map-editor tool). In addition,
## extract the groups as strings and drop the original diagnosis
## column.
spells_of_interest <- valid_icd %>%
    mutate(group = group_string(diagnosis)) %>%
    filter(group != "") %>%
    select(-diagnosis)
