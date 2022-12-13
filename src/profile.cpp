#include <Rcpp.h>

// Disable this file when compiling
// without gperftools
#ifndef NO_GPERFTOOLS
#include "gperftools/profiler.h"
#endif

using namespace Rcpp;

// [[Rcpp::export]]
SEXP start_profiler(SEXP str) {
#ifndef NO_GPERFTOOLS
    ProfilerStart(as<const char*>(str));
#endif
    return R_NilValue;
}

// [[Rcpp::export]]
SEXP stop_profiler() {
#ifndef NO_GPERFTOOLS
    ProfilerStop();
#endif
    return R_NilValue;
}
