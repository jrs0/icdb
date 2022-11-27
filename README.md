
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ICB Database Library

<!-- badges: start -->

[![Docs](https://github.com/jrs0/icdb/actions/workflows/docs.yaml/badge.svg)](https://jrs0.github.io/icdb)
[![R-CMD-check](https://github.com/jrs0/icdb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jrs0/icdb/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/jrs0/icdb/branch/main/graph/badge.svg?token=VXGD77WTZI)](https://codecov.io/gh/jrs0/icdb)
<!-- badges: end -->

ICDB is a library for connecting to health care databases, with features
designed to make data analysis using these databases easier. **Most of
the library and documentation is currently incomplete**.

## Obtaining the documentation

The documentation is hosted [here](htts://jrs0.github.com/).

If you need to generate the documentation locally, clone or download
this repository, and open an R session in the icdb/ folder. Then run the
following command:

``` r
# You may need to install devtools first 
devtools::build_site()
```

Once the documentation has finished building, there will be a folder
called *docs/* in the root location of the repository containing the
generated documentation. In addition, a web page should open with the
home page of the package documentation. After you close this page, you
can access the documentation again by opening *docs/index.html* in your
web browser.

## Installation

First clone or download this repository. Open an R session in the icdb/
folder and run the following command

``` r
devtools::install()
```

Once you have installed the library, you should be able to load the ICDB
package in any other script or project to try out the features:

``` r
library(icdb)
```
