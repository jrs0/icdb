#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::NumericVector convolveCpp(const Rcpp::NumericVector & a,
				const Rcpp::NumericVector & b) {
    // int na = a.size(), nb = b.size();
    // int nab = na + nb - 1;
    // NumericVector xab(nab);
    // for (int i = 0; i < na; i++)
    //   for (int j = 0; j < nb; j++)
    //     xab[i + j] += a[i] * b[j];
    
    return Rcpp::NumericVector(2,2.0);
}
