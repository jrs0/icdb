##' Generating example data from use in the documentation and
##' for experimenting with how the library works. Data is put
##' in a folder called gendata in the current working directory.
##'
##' Any documentation examples that require data should call this
##' function before using the data in the gendata directory.
##'
##' @title Generate example data
##' @export
icdb_example <- function()
{
    gen_clean_apc("apc.db", nspells = 100)
}
