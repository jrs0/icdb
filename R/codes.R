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
get_codes <- function(codes)
{
    if ("categories" %in% names(codes))
    {
        codes$categories %>%
            purrr::map(~ get_codes(.)) %>%
            unlist()
    }
    else if ("code" %in% names(codes))
    {
        codes$code
    }
    else
    {
        stop("Each level of the codes structure must contains 'categories' or 'code'")
    }
}
