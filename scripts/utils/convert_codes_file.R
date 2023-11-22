##' File to convert old codes yaml file to new format
##'
##' Use this script to convert a codes file with the
##' old format specified by the following grammar (curly
##' braces indicate a dictionary, square braces indicate a 
##' list, the start symbol is "old_file", the only terminal
##' symbol is "string", and the ? suffix indicates an optional
##' field):
##'
##' old_file: { child: [ childitem ], groups: [ string ] }
##' childitem: 
##'   { category: string, child: [ childitem ], docs: string, 
##'     exclude?: [ string ], index: [ string ] }
##'   |
##'   { code: string, docs: string, index: [ string ]}
##'
##' You can recognise this file type by looking for the key
##' "child" in the top level of the yaml file (next to "groups").
##'
##' The new format is specified by the following grammar (start
##' symbol is "new_file", terminal symbol is "string", and 
##' the ? suffix indicates an optional field):
##'
##' new_file: { categories: [ categoryitem ], groups: [ string ]}
##' categoryitem: 
##'   { categories?: [ categoryitem ], name: string, docs: string, 
##'     exclude?: [ string ], index: [ string ] }
##
##' You can recognise this file type by looking for the key
##' "category" in the top level of the yaml file (next to "groups").
##'
##' The new format is derived from the old format by renaming the
##' "code"/"category" string key to "name", which allows the two
##' childitem variants to be merged. The key "child" is also renamed
##' to "categories" for clarity
##'

library(tidyverse)

##' Renames "code"/"category" to "name", and if the "child" key
##' exists: 
##' - recursively convert the contents of "child" to categoryterm
##' - renames "child" to "categories"
##' 
convert_childterm_to_categoryterm <- function(childterm) {
    # Rename the fields
    names(childterm) <- names(childterm) %>%
        stringr::str_replace_all(c(code="name", category="name", child="categories"))

    # If there are subcategories, fix those (recursive)
    if ("categories" %in% names(childterm)) {
        childterm$categories <- childterm$categories %>%
            purrr::map(~ convert_childterm_to_categoryterm(.x))
    }

    # Return (what is now) the categoryterm
    childterm
}


##' Convert the old codes file located at the relative path
##' old_file_name to the new format, and save it to the 
##' relative path new_file_name
convert_codes_file <- function(old_file_name, new_file_name) {
    codes_file <- yaml::read_yaml(old_file_name)

    codes_file$child <- codes_file$child %>% purrr::map(~ convert_childterm_to_categoryterm(.x))

    # Rename the top level "child" to "categories" (special case
    # because the top level only contains "child" and "groups")
    names(codes_file) <- names(codes_file) %>%
        stringr::str_replace_all(c(child="categories"))

    yaml::write_yaml(codes_file, new_file_name)
}

##' Example usage of function
convert_codes_file("../icd10_ns.yaml", "../icd10_ns_new.yaml")
