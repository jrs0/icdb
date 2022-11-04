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

##' Use this function to generate a list of case-when like statements ready
##' for use in a dplyr query to filter a diagnosis column by codes
##'
##' @title 
##' @param y The parsed list (from parse_codes) to generate the mapping from
##' @return 
##' @author 
gen_casewhen <- function(y, colname)
{
    ## Generate the case-when
    m <- setNames(names(y), y)
    m %>%
        list(names(m),m) %>%
        purrr::pmap(~ rlang::quo(!!as.name(colname) %regex% !!.y ~ !!as.name(.x))) %>%
        unname()
}

gen_filter <- function(y,colname)
{
    ## Generate the filter
    y %>%
        purrr::map(~ rlang::quo(!!as.name(colname) %regex% !!.)) %>%
        unname()
    
}

