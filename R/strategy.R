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
##' @return The tbl after reducing the columns
strategy_coalesce <- function(tbl, name)
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
##' @return The tbl after reducing
strategy_coalesce_exclude_null <- function(tbl, name)
{
    tbl %>% strategy_coalesce(name) %>%
        dplyr::filter(!is.na(!!as.name(name)))
}

##' Read ICD codes (or any other codes) from a codes configuration file and
##' remap the column values with readable hierarchical strings.
##'
##' Note that this function only currently works on logical columns with
##' only one source column.
##'
##' @title Remap a column of codes to hierarchy of strings
##' @param tbl The tbl to process
##' @param name The logical column name
##' @param codes_file The name of the codes configuration file
##' @return The tbl after reducing
##'
strategy_codes_from <- function(tbl, name, codes_file)
{
    ## Check there is only one source column
    regx <- paste0(name,"_\\d+")
    num_cols <- tbl %>%
        dplyr::select(dplyr::matches(regx)) %>%
        colnames() %>%
        length()
    if (num_cols > 1)
    {
        stop("Strategy 'codes_from' currently only works with one source_column.")
    }
    
    ## Read the codes file from the current directory, or
    ## try from extdata
    if (file.exists(codes_file))
    {
        codes_yaml <- yaml::read_yaml(codes_file)
    }
    else
    {
        codes_yaml <- yaml::read_yaml(system.file("extdata", codes_file, package="icdb"))
    }
        
    ## Parse the codes file
    codes <- parse_codes(codes_yaml)

    ## Generate the codes map
    code_map <- gen_code_map(codes)

    ## Generate the filter and casewhen statements
    col <- paste0(name,"_1") 
    cases <- gen_casewhen(code_map, col)
    flt <- gen_filter(code_map, col)
    
    ## Perform the selection and filtering operation on the column
    tbl %>% dplyr::filter(!!!flt) %>%
        dplyr::mutate(!!name := case_when(!!!cases), .keep = "unused")
}

