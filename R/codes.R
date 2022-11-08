##'
##'
NULL

##' Parse multiple codes files and combine the result
##'
##'
##' 
##' @title Parse multiple codes files
##' @param codes_files A list of filenames or folders to parse
##' @return A named list suitable for gen_code_map()
##'
get_codes <- function(codes_files)
{
    ## Empty list for the codes
    codes <- list()
    
    ## Read each listed argument to extract the codes
    for (element in codes_files)
    {
        ## Each element could be a filename or a directory
        if (fs::file_exists(element))
        {
            ## Read the codes from this file
            x <- yaml::read_yaml(filename)
            codes <- c(codes, parse_codes(x))
        }
        else if (fs::dir_exists(element))
        {
            ## For a directory, read all of the codes files
            ## present in the directory
            files <- list.files(element, pattern = ".yaml", recursive = TRUE, full.names = TRUE)
            for (f in files)
            {
                x <- yaml::read_yaml(f)                
                codes <- c(codes, parse_codes(x))
            }
        }
    }
    codes
}

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
    ## Create a list that will hold all the results
    results <- list()

    if ("categories" %in% names(codes))
    {
        ## It is important that the categories field is
        ## parsed before the codes field, in order to
        ## preserve the precedence of the code matching,
        ## as described below.
        ##
        ## When this function parses the codes in the file,
        ## it pushes codes it finds to a list in the order
        ## that the for category for loop executes, and
        ## it processes categories before codes. This means
        ## that the categories will end up in the SQL case
        ## when statement before the codes on the same
        ## level as the category tag. As a result, more
        ## specific codes will be matched before the codes
        ## specified at the top level, which act as fall
        ## back categories.
        ##
        ## The functioning of this mechanism rests on the
        ## guarantee that the SQL case-when statement
        ## evaluates matches in the order they are listed.
        ## This appears to be the case (as it should be):
        ## "https://stackoverflow.com/questions/22640981/
        ## sql-case-does-the-order-of-the-when-statements-matter".
        ##
        ## Note that it is not necessary to use a particular
        ## order for the items in the YAML file itself --
        ## putting the codes field in front of the
        ## categories field will also work.
        ##
        ## However, it is very important not to move the
        ## else statement for the codes name above this
        ## if statement body for categories.

        ## Loop over the items in the category field,
        ## and call parse_codes at each level. parse_codes
        ## should return a list of items, where each item
        ## corresponds to a category. These items are named
        ## lists, where the names are the code strings and
        ## the values are the lists of ICD codes.
        for (category_name in names(codes$categories))
        {

            ## First, check whether the exclude flag is
            ## present and set to true, ignore this category.
            ## The codes in excluded fields will not be part
            ## of the resulting code map.
            category <- codes$categories[[category_name]]
            if (!is.null(category$exclude) && category$exclude)
            {
                next
            }

            ## Looping over categories here. Each category
            ## uses parse_codes to get a flat list, and the
            ## the category name is prepended to the names
            ## of that list. This list is then added onto the
            ## end of the main results list
            res <- parse_codes(category)

            ## Generate the category name, along with any tags
            ## for this level (stored as trailing elements of
            ## the form <tagname>
            tags <- category$tags %>%
                purrr::map(~ paste0("<",.,">")) %>%
                purrr::reduce(paste0, .init = "")
            prefix <- paste0(category_name, tags)

            ## Add the category name onto the front of every
            ## item in the list
            ## TODO add check for final element here to surpress
            ## trailing dot. (Not very important).
            names(res) <- paste0(prefix, ".", names(res))

            ## Finally, add these to the (flat) list of results
            ## for this category level
            results <- c(results, res)
        }
    }

    if ("codes" %in% names(codes))
    {
        ## Return the code. Even if the codes only contains
        ## one item, it is still treated as a list
        results <- c(results, list(codes$codes))
    }

    ## Find a more elegant way to do this
    if (!("codes" %in% names(codes)) && !("categories" %in% names(codes)))
    {
        stop("Each level of the codes structure must contains 'categories' or 'codes'")
    }

    ## Return the results, which is either a list of codes (for a codes
    ## object) or a nested list of code-lists
    results
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
##' @param code_map The result of calling gen_code_map()
##' @param colname The name of the column to use in the case-when statement
##' @return A list of case when statements (expand with !!! in case_when)
##'
gen_casewhen <- function(code_map, colname)
{
    code_map %>%
        list(names(.), .) %>%
        purrr::pmap(~ rlang::expr(!!as.name(colname) %like% !!.x ~ !!.y)) %>%
        unname()
}

gen_filter <- function(code_map,colname)
{
    ## Map reduce to generate the OR list for dplyr filtering
    flt <- names(code_map) %>%
        purrr::map(~ rlang::expr(!!as.name(colname) %like% !!.)) %>%
        purrr::reduce(~ rlang::expr(!!.x || !!.y))
}

