##' Read the ICD 11 codes into a codes mapping file from localhost
##'
##' @title Parse all ICD-11 codes
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
