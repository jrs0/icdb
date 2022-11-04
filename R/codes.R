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
        ## Get the next level down
        codes$categories %>%
            purrr::map(~ parse_codes(.)) %>%
            unlist()
    }
    else if ("code" %in% names(codes))
    {
        ## Return the code
        codes$code
    }
    else
    {
        stop("Each level of the codes structure must contains 'categories' or 'code'")
    }
}
##' Use this function to generate a list of case-when like statements ready
##' for use in a dplyr query to filter a diagnosis column by codes
##'
##' @title 
##' @param y The parsed list (from parse_codes) to generate the mapping from
##' @return 
##' @author 
gen_casewhen <- function(y)
{
    ## Generate the case-when
    m <- setNames(names(y), y)
    m %>%
        list(names(m),m) %>%
        purrr::pmap(~ rlang::quo(matches(!!as.name(.y)) ~ !!as.name(.x)))    
}

gen_filter <- function(y,colname)
{
    ## Generate the filter
    y %>%
        purrr::map(~ rlang::quo(!!as.name(colname) %like% !!.)) %>%
        unname()
    
}

