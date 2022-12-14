% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codes.R
\name{gen_code_map}
\alias{gen_code_map}
\title{Generate ICD code mapping for SQL queries}
\usage{
gen_code_map(parsed_codes)
}
\arguments{
\item{parsed_codes}{The result of calling parse_codes}
}
\value{
A named character vector from LIKE patterns to the string it represents
}
\description{
This function reverses the names and values of the parsed ICD codes from
parse_codes, ready for use in the filtering and case-when operations of
the SQL queries. This function produces are named character vector,
which may map many to one (representing multiple ICD code patterns for the
same ICD code).
}
\details{
The only need for this function is that regex is not properly supported in
SQL Server. If regex is supported, remove this and just store the codes
as regex patterns in the codes yaml file.
}
