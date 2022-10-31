##'
##'
NULL

spells <- function(tbl)
{
   tbl %>% head(100) %>% icdb::run()
}
