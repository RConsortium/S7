
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
```

R7 classes have a formal definition, which includes a list of properties
and an optional validator. You create a class with `new_class()`:

``` r
range <- new_class("range",
  properties = list(
    start = double, 
    end = double
  ),
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
```

`new_class()` returns the class object, a constructor which you can use
to create objects that are instances of that type:

``` r
x <- range(start = 1, end = 10)
x
#> <range>
#> @ start:  num 1
#> @ end  :  num 10
```

R7 objects use `@` to get and set properties:

``` r
x@start
#> [1] 1
x@end <- 20
x
#> <range>
#> @ start:  num 1
#> @ end  :  num 20
```

Properties are automatically validated against their type and with the
validator specified in the class definition:

``` r
x@end <- "x"
#> Error: <range>@end must be <double>, not <character>
x@end <- -1
#> Error: <range> object is invalid:
#> - @end must be greater than or equal to @start
```

Generics are defined by `new_generic()`. The first argument is the
generic name (used in error messages) and the second defines the
arguments that will be used for dispatch. The third, and optional
argument, supplies the body of the generic. This is only needed if your
generic has additional arguments that aren’t dispatch on.

``` r
inside <- new_generic("inside", "x", function(x, y) {
  # Actually finds and calls the appropriate method
  method_call()
})
# Add a method for our class
method(inside, range) <- function(x, y) {
  y >= x@start & y <= x@end
}
inside
#> <R7_generic> function (x, y)  with 1 methods:
#> 1: method(inside, range)

inside(x, c(0, 5, 10, 15))
#> [1] FALSE  TRUE  TRUE  TRUE
```

You can also use `method<-` to register methods for R7 objects on S3 or
S4 generics:

``` r
method(format, range) <- function(x, ...) {
  paste0("[", x@start, ", ", x@end, "]")
}
format(x)
#> [1] "[1, 20]"

method(mean, range) <- function(x, ...) {
  (x@start + x@end) / 2
}
mean(x)
#> [1] 10.5
```

Learn more in `vignettte("R7")`.
