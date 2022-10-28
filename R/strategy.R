##' This file contains reduce strategies for mapped table objects. How these strategies work is important,
##' because they perform data processing operations, and change the information returned by the database.
##' Each strategy is documented, and new strategies can be added easily. To use a strategy, define a
##' new strategy function according to the examples here, and then put the name of the strategy in the
##' mapping.yaml file for a logical column.
##'
##' All strategy functions take a tibble as the first argument, and the name of a logical column as the
##' second argument. The function returns a tibble that is the result of applying the reduce operation
##' to the given logical column.
##'
##' Before the reduce operation, a logical column consists of N tibble columns, labelled <name>_1,
##' <name>_2, ..., <name>_N. The <name> is defined in the mapping.yaml file, and the numbers come from
##' enumerating the physical columns in the order listed in the mapping.yaml file. The result of
##' applying a strategy is to reduce these columns down to a single column, called <name>, by drawing
##' on information provided in each physical column.
##'
##' The name argument (the second argument to the strategy functions) is not quoted (unlike regular
##' dplyr-style functions) -- it contains a character vector of length 1, containing the logical column
##' name.
##'
NULL

##' Reduce a logical column by the equivalent strategy, which treats elements
##' of the logical column as expressing the same information. A value is placed
##' in the logical output column if it exists in at least one of the member
##' columns. If two columns contain valid data, the columns are checked for
##' equality, and an error is thrown if the columns disagree.
##' 
##' @title Reduce equivalent columns
##' @param tbl An input dplyr::tbl
##' @param name
##' @return The tbl after reducing the columns
strategy_equivalent <- function(tbl, name)
{
    ## Get the member column names
    regx <- paste0(name,"_\\d+")
    tbl %>% dplyr::select(dplyr::matches(regx)) %>%
        dplyr::transmute(!!name := case_when(
                               !is.null(start_1) ~ start_1,
                               !is.null(start_2) ~ start_2,
                               !is.null(start_3) ~ start_3
                           ))    
}

