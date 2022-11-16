icd_apc <- function()
{
    conf <- yaml::read_yaml(system.file("extdata", "cred.secret.yaml", package = "icdb"))
    url <- conf$token_endpoint
    payload <- conf[-token_endpoint]
    
    ## get the OAUTH2 token
    r <- httr::POST(url = url, quer
    
    r = requests.post(token_endpoint, data=payload, verify=False).json()
    token = r['access_token']


    ## access ICD API

    uri = 'https://id.who.int/icd/entity'

    ## HTTP header fields to set
    headers = {'Authorization':  'Bearer '+token, 
        'Accept': 'application/json', 
        'Accept-Language': 'en',
        'API-Version': 'v2'}
    
    ## make request           
    r = requests.get(uri, headers=headers, verify=False)

    ## print the result
    print (r.text)
}
