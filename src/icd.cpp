#include <Rcpp.h>
#include <string>

//' Implementation of the ICD-10 parser for vectors of ICD-10 strings
//'
//' This function is the faster implementation of the ICD-10 parser.
//' The input is a vector of strings to be parsed, and the output is
//' a list of three vectors (the same length as the input) containing
//' information about the parsed codes.
//' 
//' @param str The input character vector of strings that should be
//' parsed
//' @param codes_file The path to a codes definition file containing
//' the ICD-10 codes and groups
//' @return A named list containing indices, type and groups
//' 
// [[Rcpp::export]]
Rcpp::List new_icd10_impl(const Rcpp::CharacterVector & str,
			  const std::string & codes_file) {

    return Rcpp::List::create(Rcpp::_("indices") = "indices",
			      Rcpp::_["type"] = "type",
			      Rcpp::_["groups"] = "groups");
}
