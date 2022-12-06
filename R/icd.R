##'
##' @importFrom vctrs vec_ptype_abbr
##'
NULL

##' This function takes a string and searches the codes
##' structure to find a match, returning the list of
##' indices defining the location of the code within
##' the structure. The purpose of the indices is to
##' store them in an icd10 object, allowing information
##' about the code to be queried. 
##'
##' Return type codes: 0 for success; 1 for empty
##' string; 2 for invalid code; 
##' 
##' @title Parse an ICD-10 code string
##' @param str The string to parse
##' @param codes The reference codes structure
##' @param groups The current set of groups for this code
##' @return A named list containing indices (a list)
##' and type (containing the status of the parse).
##' 
icd10_str_to_indices <- function(str, codes, groups)
{
    ## Check for empty string. Return -1
    ## if empty
    if (grepl("^\\s*$", str))
    {
        rlang::abort("error_invalid",
                     message = "Whitespace is not a valid ICD-10 code",
                     result = list(
                         indices = list(),
                         type = c(1),
                         trailing = str,
                         groups = list()
                     ))
    }

    ## TODO: the first thing to do to improve performance
    ## would be to implement detect_index as a binary
    ## search.
    
    ## Look through the index keys at the current level
    ## and find the position of the code
    position <- codes %>%
        purrr::map("index") %>%
        ## to obtain the first TRUE, which is
        ## the category that str is contained in
        purrr::detect_index(function(x) {

            ## Truncate the str to the same length
            ## as the start and end codes, because
            ## the end of the range is interpreted
            ## as anything beginning with this
            ## string
            trunc <- substr(str,1,nchar(x[[1]]))
            if (length(x) == 2) {
                ## If the index is a range, check that
                ## str lies in the range.
                (trunc >= x[[1]]) && (trunc <= x[[2]])
            }
            else
            {
                ## If the index is a single item,
                ## truncate str to the length of
                ## x and compare for equality
                trunc == x[[1]]
            }
        })
    
    ## If position is 0, then a match was not found. This
    ## means that the str is not a valid member of any member
    ## of this level, so it is not a valid code.
    if (position == 0)
    {
        rlang::abort("error_invalid",
                     message = "ICD-10 code was not valid",
                     result = list(
                         indices = list(),
                         type = c(2),
                         trailing = str,
                         groups = list()
                     ))
                     
    }

    ## If you get here, the code was valid at the current
    ## level. The remainder of the function is concerned with
    ## whether the current category is the best match, or
    ## whether the next category down is better.

    ## Check for any group exclusions at this level and remove
    ## them from the current group list (note that if exclude
    ## is not present, NULL is returned, which works fine).
    groups <- setdiff(groups, codes[[position]]$exclude)
    
    if (!is.null(codes[[position]]$category))
    {   
        ## Query that category for the code indices
        res <- icd10_str_to_indices(str, codes[[position]]$child, groups)
        x <- res$indices
        t <- res$type
        s <- res$trailing
        g <- res$groups
        
        ## Code from here onwards is in the reverse pass of the
        ## call tree (i.e. we are moving up the tree now, towards
        ## more general categories). The x returned above
        ## contains a -1 if the next level down was not a better
        ## match for the code. In which case, we use the current
        ## level as the best match and return the indices to
        ## this level.
        ## TODO: this whole thing is drop where == -1, replace
        ## with one liner
        val <- tail(x, n=1)
        if (val > 0)
        {
            ## Return the entire list
            list(
                indices = c(position, x),
                type = t,
                trailing = s,
                groups = g
            )
        }
        else if (val == -1)
        {
            ## The code is not better matched by the next level
            ## down. In this case, drop 
            list(
                indices = c(position, head(x, n=-1)),
                type = t,
                trailing = s,
                groups = g
            )
        }
    }
    else if (!is.null(codes[[position]]$code))
    {
        ## This section handles two cases
        ## 1) Codes that exactly match a code leaf node
        ## 2) Codes that exactly match a code leaf node,
        ##    but also contain un-parsed trailing matter
        ## The case where a code does not match any of the
        ## code leaf nodes is handled in the detect_index

        ## Use the start of the index of the code as a pattern
        ## to search for at the start of the string
        index <- codes[[position]]$index
        pattern <- paste0("^", index)
        if (grepl(pattern, str))
        {
            ## Then the code index agrees with the string
            ## at the start. Check for trailing matter
            if (nchar(index) < nchar(str))
            {
                list(
                    indices = c(position),
                    type = 3,
                    trailing = substr(str,
                                      nchar(index)+1,
                                      nchar(str)),
                    groups = groups
                )
            }
            else
            {
                ## Exact match
                list(
                    indices = c(position),
                    type = 0,
                    groups = groups
                )
            }
        }
    }
}

##' Convert a list of indices to a list of codes
##'
##' @title Convert indices lists to ICD-10 codes
##' @param indices A list of indices (itself a list) 
##' @param codes The codes definition structure
##' @return The named list containing the corresponding code
##' or category for these indices
icd10_indices_to_code <- function(indices, codes)
{
    ## The structure of the codes file is
    ## a nested list of lists. At each level,
    ## there is a key called child, which holds
    ## the next list down. Generate the arguments
    ## for use with pluck, to descend through
    ## the nested structure in one go
    k <- indices %>%
        purrr::map(~ list(.x, "child")) %>%
        purrr::flatten() %>%
        ## Remove the final "child" key to
        ## get the entire category or code
        head(-1)
        

    ## Note the first 1 is to get down into the
    ## first level (where there is a child key)
    codes %>% purrr::chuck(!!!k)
}

icd10_load_codes <- function(codes_file)
{
    codes_def <- yaml::read_yaml(codes_file)

    ## The structure must be ordered by index at
    ## every level. Most levels is already ordered
    ## by category, which is fine except for the
    ## chapter level (where the numerical order
    ## of Roman numerals does not coincide with
    ## the lexicographical order). Another exception
    ## is U occuring after Z. The function takes
    ## a list (the contents of the child key) and
    ## returns the sorted list
    sort_level <- function(level)
    {
        ## Reorder all the child levels, if
        ## there are any
        if (!is.null(level$child))
        {
            level$child <- sort_level(level$child)
        }
        
        ## Get the sorted order of this level 
        k <- level %>%
            purrr::map("index") %>%
            unlist() %>%
            order()
        
        ## Use k to reorder the current level
        level[k]
    }

    ## Sort the codes
    codes_def$child <- sort_level(codes_def$child)

    codes_def
}

##' Create a new icd10 (S3) object from a string
##'
##' @title Make an ICD-10 object from a string
##' @param str The input string to parse
##' @param codes_file The path of the codes definition
##' file to use
##' @return The new icd10 S3 object
##' 
new_icd10 <- function(str = character(), codes_file)
{
    vctrs::vec_assert(str, character())

    ## A store mapping strings to icd10 objects
    ## to reduce computation of the same codes
    ## multiple times
    cache <- list()
    
    ## Open the file. This is a long operation, but
    ## provided this function is called in a vectorised
    ## way (i.e. str is a vector), the file will only
    ## be opened once. If it turns out to be a performace
    ## problem, it can be fixed later. The top level is
    ## a list with one item, and the main chapter level
    ## starts in the child key.
    codes_def <- icd10_load_codes(codes_file)
    codes <- codes_def$child
    groups <- codes_def$groups

    ## strip whitespace from the code, and
    ## remove any dots.
    str <- stringr::str_replace_all(str, "\\.", "") %>%
        trimws()
    
    ## Get the indices for each code
    results <- str %>%
        purrr::map(function(x)
        {
                ## code <- tryCatch(
                ##     error_invalid = function(cnd)
                ##     {
                ##         cnd$result
                ##     },
                ##     error = function(cnd)
                ##     {
                ##         ## Other error condition
                ##         list(
                ##             trailing = x,
                ##             indices = list(),
                ##             type = c(2),
                ##             groups = list()
                ##         )
                ##     },
                ##     icd10_str_to_indices(x, codes, groups)
                ## )
                ## code

            
            res <- cache[[x]]
            if (!is.null(res))
            {
                res
            }
            else
            {
                code <- tryCatch(
                    error_invalid = function(cnd)
                    {
                        cnd$result
                    },
                    error = function(cnd)
                    {
                        ## Other error condition
                        list(
                            trailing = x,
                            indices = list(),
                            type = c(2),
                            groups = list()
                        )
                    },
                    icd10_str_to_indices(x, codes, groups)
                )
                cache[[x]] <- code
                code
            }
        })
        
    indices <- results %>% purrr::map("indices")
    types <- results %>% purrr::map("type")
    groups <- results %>% purrr::map("groups")
    
    ## Get the proper name
    name <- results %>%
        purrr::map(function(x) {
            if (length(x$indices) == 0) {
                paste0("(", x$trailing, ")")
            }
            else
            {
                ## This element is either a code or
                ## a category
                elem <- icd10_indices_to_code(x$indices, codes)
                if (!is.null(elem$code)) {
                    res <- elem$code
                }
                else
                {
                    res <- elem$category
                }

                ## Now append any trailing matter, if
                ## there is any
                if (!is.null(x$trailing))
                {
                    res <- paste0(res, "(", x$trailing ,")")
                }

                ## Return the name
                res
            }
        })

    obj <- list(
        name = name,
        types = types,
        indices = indices,
        groups = groups)
    vctrs::new_rcrd(obj, class = "icdb_icd10")
}

icd10 <- function(str = character(),
                  codes_file = system.file("extdata",
                                           "icd10/icd10.yaml",
                                           package = "icdb"))
{
    str <- vctrs::vec_cast(str, character())
    new_icd10(str, codes_file)
}

is_valid <- function(x) {
  UseMethod("is_valid")
}

##' @export
is_valid.icdb_icd10 <- function(x)
{
    vctrs::field(x, "types") %>%
        purrr::map(~ .x == 0) %>%
        unlist()
    
}

get_type <- function(x)
{
    vctrs::field(x, "types") %>%
        purrr::map(function(y) {
            if (y == 2) {
                "X" ## Invalid (meaningless)
            }
            else if (y == 1)
            {
                "E" ## Empty space
            }
            else if (y == 0)
            {
                "C" ## Code (correctly parsed)
            }
            else if (y == 3)
            {
                "T" ## With trailing matter
            }
        })
}

is_icd10 <- function(x) {
  inherits(x, "icdb_icd10")
}

groups <- function(x)
{
    vctrs::field(x, "groups")
}

##' @export
format.icdb_icd10 <- function(x, ...)
{
    name <- vctrs::field(x, "name")
    type <- get_type(x)

    groups <- vctrs::field(x, "groups") %>%
        purrr::map(~ if(length(.x) > 0) {
                         paste0(" <",
                                paste(unlist(.x), collapse=", "),
                                ">")
                     }
                     else
                     {
                         ""
                     })
    
    out <- paste0( "[", type, "] ", name, groups)
    out
}


##' For this to work, I needed to import the generic.
##' This feels unnecessary -- find the proper way to
##' set a method for a generic not in the class.
##' @export
##' @param x The object to abbreviate the name of 
##' @param ... Further parameters (todo: check what for)
vec_ptype_abbr.icdb_icd10 <- function(x, ...) {
  "icd10"
}

##' Convert a character vector to an icd10 vector
##'
##' @title Convert character to icd10
##' @param vec The input character vector to parse 
##' @return The icd10 vector (same length as input)
##'
to_icd10 <- function(vec)
{
    vec %>% purrr::map(~ new_icd10(.x))
}

##' Get the authentication token for the WHO ICD API
##'
##' Call this function once to get the token, and then use it
##' in the icd10_api calls. To use this function, create
##' a file secret/icd10_cred.yaml in extdata, with credentials.
##' See the example file icd10_cred.yaml in extdata for the
##' template.
##'
##' Note: this function can be mocked for testing.
##'
##' @title Get the API token
##' @return The token (named list)
icd_api_token <- function()
{
    ## Authenticate the endpoint
    secret <- yaml::read_yaml(system.file("extdata", "secret/icd10_cred.yaml", package = "icdb"))

    token_endpoint = 'https://icdaccessmanagement.who.int/connect/token'
    payload <- c(secret, list
    (
        scope = 'icdapi_access',
        grant_type = 'client_credentials'
    ))
    token <- httr::POST(token_endpoint, body = payload)
    httr::content(token)
}

##' After authenticating, make a request to an API endpoint. Use
##'
##' Note: this function can be mocked for testing.
##'
##' @title Make a request to the WHO ICD API
##' @param token The authentication token (containing access_token key)
##' @param endpoint The URL endpoint
##' @param data A named list of the headers to send
##' @return A named list of the contents of the request
##'
icd_api_request <- function(token, endpoint, data = list())
{
    headers <-
        httr::add_headers(!!!data,
                          Authorization = paste0("Bearer ",token$access_token),
                          accept = "application/json",
                          `API-Version` = "v2",
                          `Accept-Language` = "en")

    xx <- httr::GET(url = endpoint, headers)
    if (xx$status == 401)
    {
        stop("401 returned; token invalid or expired.")
    }
    else if (xx$status == 404)
    {
        stop(paste0(xx$status, " returned; the endpoint '", endpoint, "' does not exist"))
    }
    else if (xx$status != 200)
    {
        stop(paste0(xx$status, " returned; unknown error."))
    }
    httr::content(xx)

}

##' Use this function to automatically process the ICD-10 classifications into
##' a codes definition file that can be used to process diagnosis code columns.
##' This function uses the 2016 release of ICD-10.
##'
##' Use this function to get each chapter of the ICD codes, as a nested list
##' object. For now, getting each chapter separately is a better way to avoid
##' the token timing out. The results can be combined to form a full ICD-10
##' file.
##'
##' @title Generate a codes file from the ICD-10 API
##' @param token The access token obtained from icd_api_token
##' @param release The release year (default "2016"; Options "2019",
##' "2016", "2010", "2008")
##' @param item The item to retrieve; a string appended to the end of the
##' endpoint URL. This can be a chapter or a lower level item.
##' @param endpoint The endpoint URL to request. Generated from other arguments
##' if the endpoint is not provided.
##' @return A leaf or category object (named list)
##' @export
icd_api_get_codes <- function(token, release = "2016", item = "I",
                                endpoint = paste(root, release, item, sep = "/"))
{
    ## This is the base API url
    root <- "https://id.who.int/icd/release/10"

    res <- icd_api_request(token, endpoint)

    ## Each request returns a named list containing the contents of this
    ## level of the ICD hierarchy. The two most important fields are `@value`,
    ## (which is inside a key called `title`),
    ## which contains the text description of this level; and `child`, which
    ## is a list of ICD objects directly beneath this node in the tree. Each
    ## element of `child` is a URL endpoint, which can be used in another call
    ## to this function.
    ##
    ## At leaf nodes, the `child` key is missing. This signifies that the lowest
    ## level (a particular code) has been reached.
    ##
    ## If there is a code field, then it contains either a code range
    ## (e.g. I20-I25) for a non-leaf node, or it contains an ICD code
    ## (e.g. I22.0).
    ##
    ## Two other important keys, which are not currently used for anything,
    ## are the `inclusion` and `exclusion` lists for a particular node.

    if (is.null(res$child))
    {
        ## Then the current node is a leaf node. Return the map
        ## to be stored at the leaf level
        message("Fetched code '", res$code, "'")
        list(code = res$code,
             docs = res$title$`@value`)
    }
    else
    {
        ## Else, loop over the child nodes
        message("Descending into category '", res$code, "'")
        list(
            category = res$code,
            docs = res$title$`@value`,
            child = res$child %>%
                sapply(function(ep) {
                    list(icd_api_get_codes(token, endpoint = ep))
                })
        )
    }
}

##' Fetch all the ICD10 code chapters into separate files.
##'
##' The function outputs files of the form icd10_I.yaml
##' where I is replaced with the chapter number.
##'
##' @title Fetch ICD-10 codes
##' @param token The access token for the API
##' @param release The release year
##' @param dir Where to put the output files
##' @export
icd_api_fetch_all <- function(token, release = "2016", dir = ".")
{
    ## This is the base API url
    endpoint <- paste("https://id.who.int/icd/release/10",
                      release, sep="/")

    ## Fetch the chapter endpoints
    ch_ep <- icd_api_request(token, endpoint)$child
    ch_names <- ch_ep %>%
        purrr::map(~ strsplit(., "/") %>% unlist() %>% tail(n=1))

    ch_names %>%
        purrr::map(function(ch) {

            ep <- paste("https://id.who.int/icd/release/10",
                        release, ch, sep="/")
            val <- icd_api_get_codes(token, endpoint = ep)

            message("Writing codes to output file")
            if (!dir.exists(dir))
            {
                dir.create(dir)
            }

            filename <- paste0("/icd10_", ch, ".yaml")
            yaml::write_yaml(val, paste(dir, filename, sep="/"))
        })

}

##' Combine icd10_.yaml files into a single file
##'
##' @title Combine ICD-10 chapter files
##' @param dir Where to look for the files to combine
##' 
##' @export
##' 
icd_combine_files <- function(dir = system.file(folder, "icd10/", package="icdb"))
{
    files <- list.files(dir, pattern = "^icd10_(.)*.yaml$", full.names = TRUE)

    xx <- list(category = "ICD-10",
               docs = "ICD-10 codes, 2016 release",
               child = list())
    for (f in files)
    {
        yy <- yaml::read_yaml(f)
        xx$child <- c(xx$child, list(yy))
    }

    ## Write output file
    yaml::write_yaml(list(xx), paste(dir, "icd10.yaml", sep="/"))

}

##' To facilite searching for codes in the configuration
##' file, it is important for each object (category or code)
##' to store a range of codes that it contains. R supports
##' lexicographical comparison of characters by default,
##' so all that is required is to store a pair representing
##' the start of the range and the end of the range. This
##' function adds an index key to the structure passed as
##' the argument.
##'
##' The index key represents the first allowable code in
##' the category. This function replaces the code key in
##' the leaf nodes with an index, which contains the code
##' at that level.
##'
##' @title Index a codes definition structure
##' @return The codes structure with indices (a nested list)
##' @param codes The input nested list of codes
icd10_index_codes <- function(codes)
{
    ## For the new codes structure
    result <- list()
    
    for (object in codes)
    {
        if (!is.null(object$category))
        {
            ## Process the child objects
            object$child <- icd10_index_codes(object$child)

            ## Check if the category is a code range or
            ## a code, meaning it starts with a capital
            ## letter followed by a number, e.g.
            ## A00-A03 or I22. If the category is not
            ## in this form, then copy the value of
            ## the first index one level down (this
            ## is valid because of the order of evaulation
            ## of this function -- inside to out).
            if (grepl("[A-Z][0-9]", object$category))
            {
                ## Store the index as a range (start
                ## and end inclusive).
                object$index <- object$category %>%
                    stringr::str_split("-") %>%
                    unlist()
            }
            else
            {
                ## Object is a chapter. Copy the first
                ## index from one level down, using the
                ## start and end values for the range.

                ## Before doing that, the indexes must
                ## be sorted
                ## Get the sorted order of this level 
                k <- object$child %>%
                    ## Get the first value (the starting
                    ## value) of each range and sort
                    ## by that
                    purrr::map(~ .x$index[[1]]) %>%
                    unlist() %>%
                    order()

                ## Use the index k to reorder the level
                reordered <- object$child[k]

                ## Compute the range start and end based on the
                ## reordered list
                N <- length(reordered)
                object$index <- c(
                    ## Start of first range
                    reordered %>% purrr::chuck(1, "index", 1), 
                    ## End of last range
                    reordered %>%
                    purrr::chuck(N, "index", 2)
                )
            }


        }
        else if (!is.null(object$code))
        {
            object$index <- object$code %>%
                stringr::str_replace_all("\\.", "")
        }
        else
        {
            stop("Expected category or codes key in codes definition object")
        }

        result <- c(result, list(object))
    }

    ## Return the copy of the structure with indices
    result
}
