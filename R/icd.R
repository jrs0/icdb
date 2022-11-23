##'
##'
NULL

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
icd_api_get_token <- function()
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
    httr::content(xx)
}

icd10_api <- function(token, endpoint = "https://id.who.int/icd/entity")
{    
    
    res <- icd_api_request(token, endpoint)
    print(res)
    stop()
    rr <- httr::content(xx, "parsed")

    print(rr)
    stop()
    
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
