
<!-- README.md is generated from README.Rmd. Please edit that file -->

# icdb (ICB-DB)

<!-- badges: start -->
<!-- badges: end -->

The goal of icdb is to …

## Installation

You can install the development version of icdb like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(icdb)
#> 
#> Attaching package: 'icdb'
#> The following object is masked from 'package:base':
#> 
#>     table
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

## Code completedness

The functions in the package are labelled according to a 5 point scale
(0-4), according to the following criteria:

- 0: An initial version of the function has been written, but lacks
  either documentation or testing
- 1: The function has an initial version that also has initial
  documentation and is covered by some preliminary tests
- 2: To be confirmed…
- 3: To be confirmed…
- 4: To be confirmed…

These scores are provided at the end of the documnentation for each
function as a line of the form “doneness: 2/4”. You can take this as an
indication of what kind of state the function is in, and what needs most
work.

If there is no doneness label, assume that the function is incomplete.

## Todo list

Here is a list of improvements that need to be made:

- Connect to the server, not a database. Arrange things so that the user
  can autocomplete databases before tables. Try to make it work so that
  the same database connection can be used in multiple dplyr pipe
  operations.
- Flush the level1 cache to the disk at some point (i.e. when the
  session ends) to shorten the load time next session. Make sure there
  is a clear way to disable this (it might not be a desirable default).
- Need to try to get autocomplete working in every context it makes
  sense (col names etc.).
- Look into replacing dbSendQuery with dbGetQuery for simplicity
