
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ICB Database Library

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/jrs0/icdb/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jrs0/icdb?branch=main)
<!-- badges: end -->

ICDB is a library for connecting to health care databases, with features
designed to make data analysis using these databases easier.

**The library is not fully developed yet. Expect the interface and other
features to change**

***NOTE:*** ‘Server’ has been renamed to ‘server’, and ‘MappedSrv’ is
now ‘mapped\_srv’.

## Obtaining the documentation

The first step to using the library is to find a copy of the
documentation. To generate the documentation, clone or download this
repository, and open the ICDB project in RStudio. Then run the following
command:

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

Look in the *Articles* tab for information about getting started, and
other usage guides.

## Installation

First clone or download this repository. Open the package in RStudio and
run the following command

``` r
devtools::install()
```

Once you have installed the library, you should be able to load the ICDB
package in any other script or project to try out the features:

``` r
library(icdb)
```

See the main documentation for how to use the library.
