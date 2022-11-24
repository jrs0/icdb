##'
##' @importFrom vctrs vec_ptype_abbr
##'
NULL

##' Create a new icd10 (S3) object from a string
##'
##' @title Make an ICD-10 object from a string
##' @param str The input string to parse
##' @param mapping The ICD-10 mapping file to use
##' @return The new icd10 S3 object
##' 
new_icd10 <- function(str = character(), mapping = system.file("extdata", "icd10/icd10.yaml", package = "icdb"))
{
    ## Open 
    
    vctrs::vec_assert(str, character())

    ## strip whitespace from around the code
    str <- trimws(str)

    ## The icd10 class stores the meaning of a code
    ## with reference to a particular code definition
    ## file. This file is organised as a nested list
    ## of lists, meaning that a particular item in the
    ## file can be referenced by a vector of integers,
    ## which represent the indices at each level of the
    ## hierarchy.
    
    ## The object is a named list
    data <- list(
        code = str,
        indices = 
    )
    
    vctrs::new_vctr(str, class = "icdb_icd10")
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
    paste0("FMT_",x)
}


##' For this to work, I needed to import the generic.
##' This feels unnecessary -- find the proper way to
##' set a method for a generic not in the class.
##' @export
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
##' 
parse_icd10 <- function(path, output = "icd10.yaml")
{
    ## Read the file, and split the code based on the .
    tbl <- readr::read_tsv(path) %>%
        tidyr::separate(CODE, into = c("code0", "code1"), sep = "\\.") 

    ## Top level
    top <- tbl %>%
        dplyr::filter(is.na(code1))
        
    ## Create the top level codes
    codes <- list(categories = list())
    
    fn <- function(code, description)
    {
        ## Make the next level down
        bottom <- tbl %>%
            dplyr::filter(code0 == code,
                          !is.na(code1)) %>%
            dplyr::select(code1, DESCRIPTION, ALT_CODE)

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
        dplyr::select(code0, DESCRIPTION) %>%
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
