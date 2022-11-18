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

icd_api <- function()
{
    ## Authenticate the endpoint
    secret <- yaml::read_yaml(system.file("extdata", "icd-api.secret.yaml", package = "icdb"))

    print(secret)
    stop()
    
    r <- httr::POST('https://icdaccessmanagement.who.int/connect/token',
                    body = secret)
    
    ##url <- conf$token_endpoint
    ##payload <- conf[-token_endpoint]
    
    ## ## get the OAUTH2 token
    ## r <- httr::POST(url = url, quer
    
    ## r = requests.post(token_endpoint, data=payload, verify=False).json()
    ## token = r['access_token']


    ## access ICD API
    
    
    ## make request           
    #r = requests.get(uri, headers=headers, verify=False)

    ## print the result
    r
}
