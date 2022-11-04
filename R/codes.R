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
get_codes <- function(codes, prefix = "")
{
    result = list()
    if ("categories" %in% names(codes))
    {
        sub <- codes$categories
        for (name in names(sub))
        {
            ## This returns a list of the codes
            ## at this level. Prepend all the names
            ## with the prefix for this level
            sub_result <- get_codes(sub[[name]], name)
            
            print(name)
            
            result <- c(result, sub_result)
        }
        print(sub)
        stop()
    }
    else if ("code" %in% names(codes))
    {
        res <- codes$code
    }
    else
    {
        stop("Each level of the codes structure must contains 'categories' or 'code'")
    }
}
