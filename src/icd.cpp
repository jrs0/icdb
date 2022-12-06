#include <Rcpp.h>
#include <string>


// [[Rcpp::export]]
Rcpp::List new_icd10_impl(const Rcpp::CharacterVector & str,
			  const std::string & codes_file) {

    return Rcpp::List::create(Rcpp::Named("str") = str,
			      Rcpp::_["codes_file"] = codes_file);
}
