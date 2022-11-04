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
get_codes <- function(codes, result = list())
{
    if ("categories" %in% names(codes))
    {

        
        ## Descend all the way to the leaf nodes and
        ## then process recursion in return values. Sub
        ## is a list of lists, one for each 
        sub <- codes$categories %>%
            purrr::map(~ get_codes(., result))

        print(sub)
        stop("ASd")
        
        ## Assume sub is a named list. Go through it
        ## prepending the names at this level to the
        ## names in the list
        names(sub) %>% purrr::map(paste0(names(codes$categories,
                                               ".", names(sub))))

        ## Append the current result for this level
        
                                  
                                  
        
        ## for (name in names(code_list))
        ## {
            
        ## }
        

        ## ## This function generates a list where each name
        ## ## is a dot separated string with the labels in the
        ## ## yaml file (for each code), and the values are
        ## ## list of ICD10 codes corresponding to that level.
        ## parse <- function(code_list, prefix = "")
        ## {
        ##     if (is.list(code_list))
        ##     {
        ##         ## Then generate names 
        ##         for (name in names(code_list))
        ##         {
        ##             full <- paste0(prefix, ".", name) 
        ##         }
        ##     }
        ##     else
        ##     {

        ##     }
        ## }

        ##parse(code_list)
    }
    else if ("code" %in% names(codes))
    {
        result <- codes$code
    }
    else
    {
        stop("Each level of the codes structure must contains 'categories' or 'code'")
    }
    result
}
