
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ICB Database Library

<!-- badges: start -->

[![Docs](https://github.com/jrs0/icdb/actions/workflows/docs.yaml/badge.svg)](https://jrs0.github.io/icdb)
[![R-CMD-check](https://github.com/jrs0/icdb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jrs0/icdb/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/jrs0/icdb/branch/main/graph/badge.svg?token=VXGD77WTZI)](https://codecov.io/gh/jrs0/icdb)
<!-- badges: end -->

**This project is not maintained**

ICDB is a library for connecting to health care databases, with features
designed to make data analysis using these databases easier.

## Obtaining the documentation

To generate the documentation, clone or download this repository, and
open an R session in the `icdb/` folder. Run the following command:

``` r
# You need to install devtools first
devtools::build_site()
```

Once the documentation has finished building, there will be a folder
called *docs/* in the root location of the repository containing the
generated documentation. In addition, a web page should open with the
home page of the package documentation. After you close this page, you
can access the documentation again by opening *docs/index.html* in your
web browser.

## Installation

The library should install directly from the GitHub repo using the
following command (you will need to ensure that `devtools` is installed
first):

``` r
devtools::install_github("https://github.com/jrs0/icdb")
```

Once you have installed the library, you should be able to load the ICDB
package in any other script or project to try out the features:

``` r
library(icdb)
```
