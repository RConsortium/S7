
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R7

<!-- badges: start -->

[![R-CMD-check](https://github.com/RConsortium/OOP-WG/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RConsortium/OOP-WG/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/RConsortium/OOP-WG/branch/master/graph/badge.svg)](https://codecov.io/gh/RConsortium/OOP-WG?branch=master)
<!-- badges: end -->

The R7 package is a new OOP system designed to be a successor to S3 and
S4. It it is designed and implemented collaboratively by the RConsortium
Object-oriented Programming Working Group, which includes
representatives from R-Core, BioConductor, RStudio/tidyverse, and the
wider R community.

## Installation

The long-term goal of this project is to merge R7 in to base R. For now,
you can experiment with the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("r-consortium/OOP-WG")
```

## Usage

``` r
library(R7)

# Define a class
Range <- new_class("Range",
  properties = list(start = class_numeric, end = class_numeric),
  validator = function(self) {
    if (length(self@start) != 1) {
      "@start must be length 1"
    } else if (length(self@end) != 1) {
      "@end must be length 1"
    } else if (self@end < self@start) {
      "@end must be greater than or equal to @start"
    }
  }
)

# Create an object from that class
x <- Range(start = 1, end = 10)
x
#> <Range>
#>  @ start: num 1
#>  @ end  : num 10

# Get and set properties
x@start
#> [1] 1
x@end <- 20
x
#> <Range>
#>  @ start: num 1
#>  @ end  : num 20

# Can't set invalid properties
x@end <- "x"
#> Error: <Range>@end must be <integer> or <double>, not <character>
x@end <- -1
#> Error: <Range> object is invalid:
#> - @end must be greater than or equal to @start

# Define methods for existing S3 generics
method(format, Range) <- function(x, ...) paste0("[", x@start, ", ", x@end, "]")
format(x)
#> [1] "[1, 20]"

method(mean, Range) <- function(x, ...) (x@start + x@end) / 2
mean(x)
#> [1] 10.5

# Create a new generic
inside <- new_generic("inside", "x", function(x, y) {
  # Actually finds and calls the appropriate method
  method_call()
})
# Add a method for our class
method(inside, Range) <- function(x, y) y >= x@start & y <= x@end
inside
#> <R7_generic> function (x, y)  with 1 methods:
#> 1: method(inside, Range)

inside(x, c(0, 5, 10, 15))
#> [1] FALSE  TRUE  TRUE  TRUE
```
