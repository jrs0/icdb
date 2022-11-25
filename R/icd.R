##'
##' @importFrom vctrs vec_ptype_abbr
##'
NULL

## This is what you want !
## t[[1]]$child %>% purrr::map("category")


##' Search the ICD-10 codes definition structure
##' to find the indices that identify a particular
##' ICD-10 code.
##'
##' An ICD10 string is a four-character
##' code like "C71.0". In the database, the
##' dot may be missing, there may be a trailing
##' dash (for filler codes, which should be X,
##' or any other high-level category code),
##' and there may be trailing matter (such as
##' dagger or asterisk codes).
##' 
##' Even though all the information about a code
##' is present in the string, it is necessary to
##' search the codes file to find the location
##' of the code.3
##'
##' Each level of the codes file has a category
##' or a codes key, with structure as follows:
##'
##' category -> category +  -> category -> code
##' (chapter)   (code_range)   (triple)    (ICD-10)
##' I           A00-A09        A00         A00.0
##'
##' The + after the code range indicates that
##' there may be an arbitrary number of code range
##' levels, involving increasingly nested levels
##' of the codes file. A code_range is identified
##' by the presence of a dash in the category.
##' 
##' @title Identify an ICD-10 code.
##' @param str The code string (from the database) to
##' search for (with dots removed)
##' @param codes The codes definition structure
##' @return A vector of indices identifying the code
##' 
icd10_str_to_indices <- function(str, codes)
{
    ## codes is a list of objects that either
    ## contain a category key or a code key.

    ## Look through the index keys at the current level
    ## and find the position of the code
    position <- codes %>%
        purrr::map("index") %>%
        ## to obtain the first TRUE, which is
        ## the category that str is contained in
        purrr::detect_index(function(x) {
            if (length(x) == 2) {
                ## If the index is a range, check that
                ## str lies in the range. The range is
                ## of the form [start, end). The end
                ## element is open, as per the indexing
                ## function (which lexicographically
                ## increments the end of the range as
                ## stated in the ICD-10 codes).
                (str >= x[[1]]) && (str < x[[2]])
            }
            else
            {
                ## If the index is a single item,
                ## truncate str to the length of
                ## x and compare for equality
                pattern <- paste0("^", x)
                grepl(pattern, str)
            }
        })
    
    ## If position is 0, then a match was not found. This
    ## means that the str is not a valid member of any member
    ## of this level, so it is not a valid code.
    if (position == 0)
    {
        stop("'", str, "' is not a valid ICD-10 code. ")
    }

    ## If you get here, the code was valid at the current
    ## level. The remainder of the function is concerned with
    ## whether the current category is the best match, or
    ## whether the next category down is better.
    
    if (!is.null(codes[[position]]$category))
    {   
        ## If you get here, the code was valid for this category
        ## If the category was a match, then
        ## Query that category for the code
        x <- icd10_str_to_indices(str, codes[[position]]$child)
        
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
            c(position, x)
        }
        else if (val == -1)
        {
            ## The code is not better matched by the next level
            ## down. In this case, drop 
            c(position, head(res, n=-1))
        }
    }
    else if (!is.null(codes[[position]]$code))
    {
        ## An exact match is required, else return -1 to signify
        ## failed match. TODO Add the valid code match patterns
        ## here
        if (str != codes[[position]]$index)
        {
            c(-1)
        }
        else
        {
            ## Else, the code matched. Return the position
            ## in the current block
            c(position)
        }
        
    }
    else
    {
        stop("Expected to find code or category in ICD-10 list element")
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

icd10_load_codes <- function(file = system.file("extdata",
                                                "icd10/icd10_index.yaml",
                                                package = "icdb"))
{
    codes <- yaml::read_yaml(file)

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

    ## Sprt the codes
    sort_level(codes)
}

##' Create a new icd10 (S3) object from a string
##'
##' @title Make an ICD-10 object from a string
##' @param str The input string to parse
##' @param codes The ICD-10 codes definition file to use
##' @return The new icd10 S3 object
##' 
new_icd10 <- function(str = character())
{
    vctrs::vec_assert(str, character())

    ## Open the file. This is a long operation, but
    ## provided this function is called in a vectorised
    ## way (i.e. str is a vector), the file will only
    ## be opened once. If it turns out to be a performace
    ## problem, it can be fixed later. The top level is
    ## a list with one item, and the main chapter level
    ## starts in the child key.
    codes <- icd10_load_codes()

    ## strip whitespace from the code, and
    ## remove any dots.
    str <- stringr::str_replace_all(str, "\\.", "")

    ## Get the indices for each code
    indices <- str %>%
        purrr::map(~ icd10_str_to_indices(.x, codes))

    ## Get the proper name
    name <- indices %>%
        purrr::map(~ icd10_indices_to_code(.x, codes)) %>%
        purrr::map("code") %>%
        unlist()

    obj <- list(name, indices) %>%
        purrr::pmap(~ list(.x, .y))
    
    vctrs::new_vctr(obj, class = "icdb_icd10")
}

icd10 <- function(str = character())
{
    str <- vctrs::vec_cast(str, character())
    new_icd10(str)
}

is_icd10 <- function(x) {
  inherits(x, "icdb_icd10")
}

##' @export
format.icdb_icd10 <- function(x, ...) {
    x %>% purrr::map(1) %>% unlist()
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

##' Parse an ICD-10 codes file from the NHS Digital (TRUD) from the
##' "NHS ICD-10 5th Edition data files" item. The function converts the
##' codes to a codes definition file suitable for use in a mapped
##' server.
##'
##' @title Parse ICD-10 codes
##' @param path The input file path (tab separated)
##' @param output The filename of the output definition file
parse_icd10 <- function(path, output = "icd10.yaml")
{
    ## Read the file, and split the code based on the .
    tbl <- readr::read_tsv(path) %>%
        tidyr::separate(.data$CODE, into = c("code0", "code1"), sep = "\\.")

    ## Top level
    top <- tbl %>%
        dplyr::filter(is.na(.data$code1))

    ## Create the top level codes
    codes <- list(categories = list())

    fn <- function(code, description)
    {
        ## Make the next level down
        bottom <- tbl %>%
            dplyr::filter(.data$code0 == code,
                          !is.na(.data$code1)) %>%
            dplyr::select(.data$code1, .data$DESCRIPTION, .data$ALT_CODE)

        categories <- bottom %>%
            purrr::pmap(~ list(docs = ..2,
                               codes = list(paste0(code,".",..1),
                                            ..3)))
        names(categories) <- bottom$code1

        ## Make the categories
        res <- list(
            docs = description,
            codes = list(code)
        )

        ## If the categories is populated, add it
        if (length(categories) > 0)
        {
            res$categories <- categories
        }

        res
    }

    codes$categories <- top %>%
        dplyr::select(.data$code0, .data$DESCRIPTION) %>%
        purrr::pmap(~ fn(.x, .y))
    names(codes$categories) <- top$code0

    ## Write the output file
    yaml::write_yaml(codes, file = output)

}

##' Read the ICD 11 codes into a codes mapping file from localhost
##'
##' @title Parse all ICD-11 codes
##' @param endpoint The top-level endpoint to start the search
parse_icd11 <- function(endpoint = "http://localhost/icd/entity")
{
    xx <- httr::GET(url = endpoint, httr::add_headers(accept = "application/json",
                                                `API-Version` = "v2",
                                                `Accept-Language` = "en"))
    rr <- httr::content(xx, "parsed")

    print(rr)

    ## Get documentation
    pp <- list(docs = rr$title$`@value`)

    ## Parse all the child entities
    ss <- list()
    for (url in rr$child)
    {
        ## URLs use the remote host, replace this with localhost
        url <- stringr::str_replace(url, pattern = "id.who.int", replacement = "localhost")

        ##ss[[janitor::make_clean_names
    }
}

##' Get the authentication token for the WHO ICD API
##'
##' Call this function once to get the token, and then use it
##' in the icd10_api calls
##'
##' Note: this function can be mocked for testing.
##'
##' @title Get the API token
##' @return The token (named list)
icd_api_token <- function()
{
    ## Authenticate the endpoint
    secret <- yaml::read_yaml(system.file("extdata", "secret/icd10-cred.yaml", package = "icdb"))

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
##' @param release The release year (see )
##'
##' @export
icd_api_fetch_all <- function(token, release = "2016")
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
            yaml::write_yaml(val, paste0("icd10_", ch, ".yaml"))
        })

}

##' Combine icd10_.yaml files into a single file
##'
##' @title Combine ICD-10 chapter files
##'
##' @export
icd_combine_files <- function()
{
    files <- list.files(system.file("extdata", "icd10/", package="icdb"),
                        pattern = "^icd10_(.)*.yaml$", full.names = TRUE)

    xx <- list(category = "ICD-10",
               docs = "ICD-10 codes, 2016 release",
               child = list())
    for (f in files)
    {
        yy <- yaml::read_yaml(f)
        xx$child <- c(xx$child, list(yy))
    }

    ## Write output file
    yaml::write_yaml(xx, "icd10.yaml")

}

##' Increment the category at the current level (i.e.
##' X34 -> X35, not e.g. X3400...0001)
##' 
##' @title Lexicographically increment an ICD-10 category
##' @param word The word to increment (of the form X43)
##' @return The next word (X44)
icd10_lex_inc <- function(word)
{
    ## Assume format X34 (Letter-Num-Num)
    num <- word %>% stringr::str_replace(".","") %>%
        as.integer()
     
    word %>%
        stringr::str_replace("[0-9]+",
                             as.character(num+1))
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

                ## Lexicographically increment the
                ## end of the range to allow codes
                ## beyond the end (for example,
                ## B34.9 must be included in the
                ## category that ends B34)
                if (length(object$index) == 2)
                {
                    object$index[[2]] <- object$index[[2]] %>%
                        icd10_lex_inc()
                }
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
