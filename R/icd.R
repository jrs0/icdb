##'
##' @importFrom vctrs vec_ptype_abbr
##'
NULL

##' The codes structure must be ordered by index at
##' every level. Most levels is already ordered
##' by category, which is fine except for the
##' chapter level (where the numerical order
##' of Roman numerals does not coincide with
##' the lexicographical order). Another exception
##' is U occuring after Z. The function takes
##' a category and sorts the child array (if it
##' exists), retuning the modified category.
##'
##' This function calls itself recursively and
##' modifies all child categories inside the
##' argument cat.
##'
##' @title Reorder the child categories in a category
##' @param cat The category to reorder 
##' @return The modified category
icd10_sort_by_index <- function(cat)
{
    ## Only perform the reordering if this
    ## is a proper category -- do not reorder
    ## anything for leaf (code) nodes
    if (!is.null(cat$categories))
    {
        ## Reorder all the child levels, if
        ## there are any
        for (n in seq_along(cat$categories))
        {
            cat$categories[[n]] <- icd10_sort_by_index(cat$categories[[n]])
        }

        ## Get the sorted order of this list of
        ## categories. The intention here is to sort
        ## by the first element of the index (the
        ## unlist is used to flatten the resulting list)
        k <- cat$categories %>%
            purrr::map("index") %>%
            purrr::map(~ .[[1]]) %>%
            unlist() %>%
            order()
        
        ## Use k to reorder the current level
        cat$categories <- cat$categories[k]
    }
    
    ## Return the modified category
    cat
}


## This object is used to avoid rereading the same
## (quite large) codes file every time icd10 is used.
CodesDefCache <- R6::R6Class(
                      "CodesCache",
                      public = list(
                          codes_file = "",
                          codes_def = NULL,
                          load_codes = function(codes_file)
                          {
                              ## The false disables the cache for now
                              if (codes_file == self$codes_file)
                              {
                                  ## You have previously loaded the codes
                                  ## Just return it
                                  self$codes_def
                              }
                              else
                              {
                                  ## You need to reread the codes from the disk
                                  codes_def <- yaml::read_yaml(codes_file)
                                  self$codes_def <- icd10_sort_by_index(codes_def)
                                  ## Set the codes file
                                  self$codes_file <- codes_file
                                  ## Return the codes
                                  self$codes_def
                              }
                              
                          }
                      )
                      )

## Global codes file
codes_def_cache <- CodesDefCache$new()

icd10_load_codes <- function(codes_file)
{
    codes_def_cache$load_codes(codes_file)
}

##' Create a new icd10 <-(S3) object from a string
##'
##' @title Make an ICD-10 object from a string
##' @param str The input string to parse
##' @param codes_file The path of the codes definition
##' file to use
##' @return The new icd10 S3 object
##' 
new_icd10 <- function(str = character(), codes_file)
{
    vctrs::vec_assert(str, character())
    codes_def <- icd10_load_codes(codes_file) 
    str <- stringr::str_replace_all(str, "\\.", "") %>%
        trimws()
    results <- new_icd10_impl(str, codes_def)
    vctrs::new_rcrd(results, codes_file = codes_file, class = "icdb_icd10")
}

##' An icd10 object is a vector class containing parsed ICD codes,
##' along with optional groups defined by a codes mapping file. The
##' codes are parsed from an input string, which contains ICD codes
##' that may have leading/trailing whitespace, may not contain
##' complete ICD codes, or may contain invalid codes. These codes
##' are categorised as part of the parsing process.
##'
##' @title Create a new icd10 object
##' @param str The input character vector of ICD code strings to parse
##' @param codes_file The reference codes file (defining codes and groupings)
##' @return A new icd10 vector
##' @export
icd10 <- function(str = character(),
                  codes_file = system.file("extdata",
                                           "code_groups/icd10.yaml",
                                           package = "icdb"))
{
    str <- vctrs::vec_cast(str, character())
    new_icd10(str, codes_file)
}

##' @title Return which ICD codes are valid as a logical vector
##' @param x The icd10 codes to read
##' @return A logical vector containing which codes are valid
##' @export
is_valid <- function(x) {
  UseMethod("is_valid")
}

##' @export
is_valid.icdb_icd10 <- function(x)
{
    vctrs::field(x, "types") %>%
        purrr::map(~ (.x == 0 || .x == 3)) %>%
        unlist()
}

in_any_group <- function(x) {
  UseMethod("in_any_group")
}

##' @export
in_any_group.icdb_icd10 <- function(x)
{
    vctrs::field(x, "groups") %>%
        purrr::map(~ (length(.x) > 0)) %>%
        unlist()
}

##' Convert a list of ICD10 codes to a character vector
##' Invalid ICD codes are returned as NA.
##'
##' @param x The icd10 vector object to convert 
##' @param ... Unused addition parameters
##' @export
as.character.icdb_icd10 <- function(x, ...)
{
    vctrs::field(x, "basic_name") %>%
        stringr::str_replace("NA", NA_character_)
}


##' Returns a character vector containing single letters that
##' represent the type (the parse status) of the icd10 code.
##' The valid types are: C = code correctly parsed; X = code
##' is invalid; E = code is empty string; T = code parsed
##' correctly, but contains trailing matter.
##'
##' @title Get a character vector or types of icd10 codes
##' @param x The input vector of icd10 codes to parse
##' @return A character vector or types
get_types <- function(x)
{
    vctrs::field(x, "types") %>%
        purrr::map(function(y) {
            if (is.na(y)) {
                NA ## Return NA
            }
            else if (y == 2) {
                "X" ## Invalid (meaningless)
            }
            else if (y == 1)
            {
                "E" ## Empty space
            }
            else if (y == 0)
            {
                "C" ## Code (correctly parsed)
            }
            else if (y == 3)
            {
                "T" ## With trailing matter
            }
        }) %>%
        unlist()
}

is_icd10 <- function(x) {
  inherits(x, "icdb_icd10")
}

##' Valid type
##' 
##' @title Count the number of occurances of type
##' @param x The icd10 vector to count
##' @param type Character, One of "C", "E", "X", "T" 
##' @return The total number of matching types
count_type <- function(x, type)
{
    sum(get_types(x) == type)
}

##' Get the parse statistics for a vector of icd10
##' codes. The statistics include the number of valid
##' and invalid codes, codes with trailing matter, and
##' empty strings.
##'
##' @title Get ICD-10 parse statistics
##' @param x The icd10 codes list to process
##' @return A list contains counts of each class
##'
##' @export
get_parse_stats <- function(x)
{
    list (
        valid_count = count_type(x, "C"),
        invalid_count = count_type(x, "X"),
        empty_count = count_type(x, "E"),
        trailing_count = count_type(x, "T")       
    )
}

groups <- function(x) {
    UseMethod("groups")
}

##' @export
groups.icdb_icd10 <- function(x)
{
    vctrs::field(x, "groups")
}

##' If a code has multiple groups, the resulting item in the
##' character vector is a comma separated list of groups.
##'
##' @title Extract groups character vector from icd10 object 
##' @param x The the vector of icd10 objects to obtain groups
##' from
##' @return The groups as a character vector
##' @export
group_string <- function(x)
{
    groups(x) %>%
        purrr::map(~ paste(.x, collapse=",")) %>%
        unlist()
}

name <- function(x) {
    UseMethod("name")
}

##' @export
name.icdb_icd10 <- function(x)
{
    vctrs::field(x, "name")
}

##' @export
summary.icdb_icd10 <- function(object, ...)
{
    c(get_parse_stats(object))
}

##' Check whether an icd10 code is in a particular group.
##' Suitable for use in the data masking arguments of
##' dplyr calls. The function will throw an error if
##' it is supplied a column which is not class icd10.
##'
##' @title Check if ICD-10 code is in a group 
##' @param x The icd10 codes (icd10 vector) to test
##' @param group A character vector of groups. If there
##' are multiple groups, codes will be returned for all
##' the groups listed.
##' @return TRUE where the code is in the group,
##' FALSE otherwise
##' @export
in_group <- function(x, group)
{
    stopifnot("icdb_icd10" %in% class(x))

    g <- groups({{x}})
    g %>% purrr::map(~ any(group %in% .x)) %>% unlist()
}

##' @title Binary operator version of in_groups
##' @param x The icd10 codes (icd10 vector) to test
##' @param group A character vector of groups. If there
##' are multiple groups, codes will be returned for all
##' the groups listed.
##' @export
`%in_group%` <- function(x, group) in_group(x,group)



##' Check whether an icd10 code has a particular type
##' (parse status). This function can be used in the
##' data masking argument of dplyr::filter to extract
##' codes that parsed correctly (type == "C"), empty
##' codes (type == "E"), parsed correctly but with
##' trailing matter (type == "T"), or are invalid
##' (type == "X").
##'
##' @title Check if ICD-10 codes have a particular type
##' @param x The icd10 codes (icd10 vector) to test
##' @param type Character, One of "C", "E", "X", "T" 
##' @return TRUE where the code has the specified type,
##' FALSE otherwise
##' 
##' @export 
has_type <- function(x, type)
{
    vctrs::vec_assert({{x}}, icd10())

    tt <- get_types({{x}})
    tt == type
}


##' @export
format.icdb_icd10 <- function(x, ...)
{
    name <- vctrs::field(x, "name")
    types <- get_types(x)

    groups <- vctrs::field(x, "groups") %>%
        purrr::map(~ if(length(.x) > 0) {
                         paste0(" <",
                                paste(unlist(.x), collapse=", "),
                                ">")
                     }
                     else
                     {
                         ""
                     })
    
    out <- paste0( "[", types, "] ", name, groups)
    out[is.na(types)] <- NA # Store NA only wherever the type is NA
    out
}


##' For this to work, I needed to import the generic.
##' This feels unnecessary -- find the proper way to
##' set a method for a generic not in the class.
##' @export
##' @param x The object to abbreviate the name of 
##' @param ... Further parameters (todo: check what for)
vec_ptype_abbr.icdb_icd10 <- function(x, ...) {
  "icd10"
}

##' Convert a character vector to an icd10 vector
##'
##' @title Convert character to icd10
##' @param vec The input character vector to parse 
##' @return The icd10 vector (same length as input)
##'
to_icd10 <- function(vec)
{
    vec %>% purrr::map(~ new_icd10(.x))
}

