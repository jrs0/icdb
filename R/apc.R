##'
##'
NULL

spells <- function(tbl, start = lubridate::ymd("2021-8-1"), end = lubridate::ymd("2021-8-10"))
{
    codes <- yaml::read_yaml(system.file("extdata", "acs_codes.yaml", package="icdb"))
    tbl %>%
        dplyr::filter(primary_diagnosis %in% !!get_codes(codes),
                      start >= !!start,
                      start <= !!end) %>%
        tidyr::pivot_wider(names_from=primary_diagnosis, values_from=start) %>%
        icdb::run()
        
}
