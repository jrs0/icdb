% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icd.R
\name{has_type}
\alias{has_type}
\title{Check if ICD-10 codes have a particular type}
\usage{
has_type(x, type)
}
\arguments{
\item{x}{The icd10 codes (icd10 vector) to test}

\item{type}{Character, One of "C", "E", "X", "T"}
}
\value{
TRUE where the code has the specified type,
FALSE otherwise
}
\description{
Check whether an icd10 code has a particular type
(parse status). This function can be used in the
data masking argument of dplyr::filter to extract
codes that parsed correctly (type == "C"), empty
codes (type == "E"), parsed correctly but with
trailing matter (type == "T"), or are invalid
(type == "X").
}
