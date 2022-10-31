##'
##'
NULL

spells <- function(tbl)
{
    codes <- yaml::read_yaml(system.file("extdata", "acs_codes.yaml", package="icdb"))
    ## tbl %>%
    ##     filter(primary_diagnosis %in% !!codes) %>%
    ##     dplyr::show_query()
    codes
}
