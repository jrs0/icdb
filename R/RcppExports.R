# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Implementation of the ICD-10 parser for vectors of ICD-10 strings
#'
#' This function is the faster implementation of the ICD-10 parser.
#' The input is a vector of strings to be parsed, and the output is
#' a list of three vectors (the same length as the input) containing
#' information about the parsed codes.
#' 
#' @param str The input character vector of strings that should be
#' parsed. The strings can have leading and trailing whitespace,
#' which will be removed before parsing by this function.
#' @param code_def The code definition structure, which is a nested
#' list of lists following the structure of the icd10.yaml file.
#' @return A named list containing indices, type and groups
#' 
#' 
new_icd10_impl <- function(str, code_def) {
    .Call(`_icdb_new_icd10_impl`, str, code_def)
}

start_profiler <- function(str) {
    .Call(`_icdb_start_profiler`, str)
}

stop_profiler <- function() {
    .Call(`_icdb_stop_profiler`)
}

