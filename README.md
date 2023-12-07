
<!-- README.md is generated from README.Rmd. Please edit that file -->

# S7

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/RConsortium/S7/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/RConsortium/S7/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/RConsortium/S7/branch/main/graph/badge.svg)](https://app.codecov.io/gh/RConsortium/S7?branch=main)

<!-- badges: end -->

The S7 package is a new OOP system designed to be a successor to S3 and
S4. It has been designed and implemented collaboratively by the R
Consortium Object-Oriented Programming Working Group, which includes
representatives from R-Core, Bioconductor, the tidyverse/Posit, and the
wider R community.

S7 is somewhat experimental; we are confident in the design but it has
relatively little usage in the wild currently. We hope to avoid any
major breaking changes, but reserve the right if we discover major
problems.

## Installation

The long-term goal of this project is to merge S7 in to base R. For now,
you can experiment by installing it from CRAN:

``` r
install.packages("S7")
```

## Usage

This section gives a very brief overview of the entirety of S7. Learn
more of the basics in `vignette("S7")`, generics and methods in
`vignette("generics-methods")`, classes and objects in
`vignette("classes-objects")`, and compatibility with S3 and S4 in
`vignette("compatibility")`.

``` r
library(S7)
```

### Classes and objects

S7 classes have a formal definition, which includes a list of properties
and an optional validator. Use `new_class()` to define a class:

``` r
range <- new_class("range",
  properties = list(
    start = class_double,
    end = class_double
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

`new_class()` returns the class object, which is also the constructor
you use to create instances of the class:

``` r
x <- range(start = 1, end = 10)
x
#> <range>
#>  @ start: num 1
#>  @ end  : num 10
```

### Properties

The data possessed by an object is called its **properties**. Use `@` to
get and set properties:

``` r
x@start
#> [1] 1
x@end <- 20
x
#> <range>
#>  @ start: num 1
#>  @ end  : num 20
```

Properties are automatically validated against the type declared in
`new_class()` (`double` in this case), and with the class **validator**:

``` r
x@end <- "x"
#> Error: <range>@end must be <double>, not <character>
x@end <- -1
#> Error: <range> object is invalid:
#> - @end must be greater than or equal to @start
```

### Generics and methods

Like S3 and S4, S7 uses **functional OOP** where methods belong to
**generic** functions, and method calls look like all other function
calls: `generic(object, arg2, arg3)`. This style is called functional
because from the outside it looks like a regular function call, and
internally the components are also functions.

Use `new_generic()` to create a new generic: the first argument is the
generic name (used in error messages) and the second gives the arguments
used for dispatch. The third, and optional argument, supplies the body
of the generic. This is only needed if your generic has additional
arguments that aren’t used for method dispatch.

``` r
inside <- new_generic("inside", "x")
```

Once you have a generic, you can define a method for a specific class
with `method<-`:

``` r
# Add a method for our class
method(inside, range) <- function(x, y) {
  y >= x@start & y <= x@end
}

inside(x, c(0, 5, 10, 15))
#> [1] FALSE  TRUE  TRUE  TRUE
```

You can use `method<-` to register methods for base types on S7
generics, and S7 classes on S3 or S4 generics. See
`vignette("compatibility")` for more details.
