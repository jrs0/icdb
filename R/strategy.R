## @name strategy.R
##
## This file contains reduce strategies for mapped table objects. How these strategies work is important,
## because they perform data processing operations, and change the information returned by the database.
## Each strategy is documented, and new strategies can be added easily. To use a strategy, define a
## new strategy function according to the examples here, and then put the name of the strategy in the
## mapping.yaml file for a logical column.
##
## All strategy functions take a tibble as the first argument, and the name of a logical column as the
## second argument. The function returns a tibble that is the result of applying the reduce operation
## to the given logical column.
##
## Before the reduce operation, a logical column consists of N tibble columns, labelled {name}_1,
## {name}_2, ..., {name}_N. The {name} is defined in the mapping.yaml file, and the numbers come from
## enumerating the physical columns in the order listed in the mapping.yaml file. The result of
## applying a strategy is to reduce these columns down to a single column, called {name}, by drawing
## on information provided in each physical column.
##
## The name argument (the second argument to the strategy functions) is not quoted (unlike regular
## dplyr-style functions) -- it contains a character vector of length 1, containing the logical column
## name.
##
NULL

##' Reduce a logical column by the coalesce strategy, which treats elements
##' of the logical column as expressing the same information. Physical columns
##' are ordered according to the ordering specified in the yaml file. The value
##' in the final logical column is selected from the first physical column for
##' which the value is not NULL. This is the same as the SQL COALESCE function,
##' which does the same thing.
##'
##' TODO: consider replacing this function by the built-in coalesce function
##' which probably works. Secondly, it may be a good idea to put the default
##' statement of the case when to catch issues (like everything being null).
##'
##' @title Reduce by coalescing columns
##' @param tbl An input dplyr::tbl
##' @param name The logical column name
##' @importFrom rlang := quo
##' @importFrom dplyr matches select mutate
##' @importFrom magrittr %>%
##' @importFrom purrr map
##' @return The tbl after reducing the columns
coalesce <- function(tbl, name)
{
    ## Get the member column names
    regx <- paste0(name,"_\\d+")
    cases <- tbl %>%
        dplyr::select(dplyr::matches(regx)) %>%
        colnames() %>%
        purrr::map(~ rlang::quo(!is.null(!!as.name(.)) ~ !!as.name(.)))
    tbl %>% dplyr::mutate(!!name := case_when(!!!cases), .keep = "unused")
}

##' This is the same as the coalesce strategy, but it also filters out rows
##' where the coalesced result is null. This is important for logical
##' columns where a null result would render the entire row meaningless.
##'
##' This function excludes all undefined value
##'
##' @title Reduce by coalescing columns, excluding resulting null values
##' @param tbl The tbl to process
##' @param name The logical column name
##' @importFrom magrittr %>%
##' @importFrom dplyr coalesce filter
##' @return The tbl after reducing
coalesce_exclude_null <- function(tbl, name)
{
    ## This coalesce is not from dplyr, it is from icdb
    ## (the function above). Cannot put icdb:: in front
    ## because it is not exported. Feels like there should be
    ## a way to specify it, but TODO later...
    tbl %>% coalesce(name) %>%
        dplyr::filter(!is.na(!!as.name(name)))
}

##' Read ICD codes (or any other codes) from a codes configuration file and
##' remap the column values with readable hierarchical strings.
##'
##' Note that this function only currently works on logical columns with
##' only one source column.
##'
##' Note that this function is a second layer strategy, meaning it must be
##' applied only on the resulting logical column (not the column names
##' which have a trailing _N).
##'
##' @title Remap a column of codes to hierarchy of strings
##' @param tbl The tbl to process
##' @param name The logical column name (unquoted, like in dplyr filter etc.)
##' @param codes_files A list of the names of codes configuration files
##' @importFrom magrittr %>%
##' @importFrom dplyr mutate filter case_when
##' @importFrom rlang ensym
##' @return The tbl after reducing
##'
##' @export
codes_from <- function(tbl, codes_files, name)
{
    ## Get the codes from multiple files
    codes <- get_codes(codes_files)

    ## Generate the codes map
    code_map <- gen_code_map(codes)

    ## Generate the filter and casewhen statements
    cases <- gen_casewhen(code_map, rlang::ensym(name))
    flt <- gen_filter(code_map, rlang::ensym(name))

    ## Perform the selection and filtering operation on the column
    ## The line for setting the name needs some work, but it works
    ## so I'm leaving it for now.
    ## BUG here -- %like% not found (when tbl is not an sql object)
    tbl %>% dplyr::filter(flt) %>%
        dplyr::mutate(!!as.name(rlang::ensym(name)) := dplyr::case_when(!!!cases), .keep = "unused")
}

