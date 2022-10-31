##'
##'
NULL

spells <- function(tbl)
{
    codes <- yaml::read_yaml(system.file("extdata", "acs_codes.yaml", package="icdb"))
    tbl %>%
        dplyr::filter(primary_diagnosis %in% !!get_codes(codes))
}
