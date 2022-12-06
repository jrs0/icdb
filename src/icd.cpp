#include <Rcpp.h>
#include <string>


// [[Rcpp::export]]
Rcpp::List convolveCpp(const Rcpp::NumericVector & a,
		       const Rcpp::NumericVector & b) {
    // int na = a.size(), nb = b.size();
    // int nab = na + nb - 1;
    // NumericVector xab(nab);
    // for (int i = 0; i < na; i++)
    //   for (int j = 0; j < nb; j++)
    //     xab[i + j] += a[i] * b[j];

    std::string s{"abc"}; 
    auto L{Rcpp::List::create(Rcpp::Named("name1") = s , Rcpp::_["name2"] = 1.2)};
    
    return L;
}
