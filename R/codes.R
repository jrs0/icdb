##'
##'
NULL

##' Get all the codes in a codes definition yaml file as a list.
##'
##' A codes definition file is a yaml with a tree-like structure
##' where leaves of the tree are ICD codes, and non-leaf nodes
##' are categories. Each level in the structure contains either
##' a "categories" field (for a non-leaf) or a "code" field
##' (for a leaf). All levels in the structure contains a "docs"
##' field describing what the level means.
##' 
##' @title Get a flat list of codes
##' @param codes The parsed codes definition list of lists
##' @return A named character vector containing the codes
##'
##' @export
parse_codes <- function(codes)
{
    if ("categories" %in% names(codes))
    {
        ## Create a list that will hold all the results
        results <- list()
        
        ## Loop over the items in the category field,
        ## and call parse_codes at each level. parse_codes
        ## should return a list of items, where each item
        ## corresponds to a category. These items are named
        ## lists, where the names are the code strings and
        ## the values are the lists of ICD codes.
        for (category_name in names(codes$categories))
        {
            ## Looping over categories here. Each category
            ## uses parse_codes to get a flat list, and the
            ## the category name is prepended to the names
            ## of that list. This list is then added onto the
            ## end of the main results list
            res <- parse_codes(codes$categories[[category_name]])

            ## Add the category name onto the front of every
            ## item in the list
            ## TODO add check for final element here to surpress
            ## trailing dot. (Not very important).
            names(res) <- paste0(category_name, ".", names(res))

            ## Finally, add these to the (flat) list of results
            ## for this category level
            results <- c(results, res)
        }

        ## Return the results for this category level
        results

    }
    else if ("code" %in% names(codes))
    {
        ## Return the code
        list(codes$code)
    }
    else
    {
        stop("Each level of the codes structure must contains 'categories' or 'code'")
    }
}

##' This function reverses the names and values of the parsed ICD codes from
##' parse_codes, ready for use in the filtering and case-when operations of
##' the SQL queries. This function produces are named character vector,
##' which may map many to one (representing multiple ICD code patterns for the
##' same ICD code).
##'
##' The only need for this function is that regex is not properly supported in
##' SQL Server. If regex is supported, remove this and just store the codes
##' as regex patterns in the codes yaml file.
##'
##' @title Generate ICD code mapping for SQL queries
##' @param parsed_codes The result of calling parse_codes
##' @return A named character vector from LIKE patterns to the string it represents
##' 
gen_code_map <- function(parsed_codes)
{
    result <- character()
    for (name in names(parsed_codes))
    {
        for (code in parsed_codes[[name]])
        {
            ## Reverse the list and flatten codes 
            result[[code]] <- name
        }
    }
    result
}

##' Use this function to generate a list of case-when like statements ready
##' for use in a dplyr query to filter a diagnosis column by codes
##'
##' @title Generate the case-when list to use in the dplyr query
##' @param code_map The map from gen_code_map
##' @return A list of case when statements (expand with !!! in case_when)
##' 
gen_casewhen <- function(code_map, colname)
{
    code_map %>%
        list(names(.), .) %>%
        purrr::pmap(~ rlang::quo(!!as.name(colname) %like% !!.x ~ !!.y)) %>%
        unname()
}

gen_filter <- function(code_map,colname)
{
    ## Generate the filter
    names(code_map) %>%
        purrr::map(~ rlang::quo(!!as.name(colname) %like% !!.)) %>%
        unname()
    
}

