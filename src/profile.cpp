// Disable this file when compiling
// without gperftools
#ifndef NO_GPERFTOOLS

#include <Rcpp.h>
#include "gperftools/profiler.h"

using namespace Rcpp;

// [[Rcpp::export]]
SEXP start_profiler(SEXP str) {
  ProfilerStart(as<const char*>(str));
  return R_NilValue;
}

// [[Rcpp::export]]
SEXP stop_profiler() {
  ProfilerStop();
  return R_NilValue;
}

#endif
