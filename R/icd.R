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
