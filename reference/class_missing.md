# Dispatch on a missing argument

Use `class_missing` to dispatch when the user has not supplied an
argument, i.e. it's missing in the sense of
[`missing()`](https://rdrr.io/r/base/missing.html), not in the sense of
[`is.na()`](https://rdrr.io/r/base/NA.html).

## Usage

``` r
class_missing
```

## Value

Sentinel objects used for special types of dispatch.

## Examples

``` r
foo <- new_generic("foo", "x")
method(foo, class_numeric) <- function(x) "number"
method(foo, class_missing) <- function(x) "missing"
method(foo, class_any) <- function(x) "fallback"

foo(1)
#> [1] "number"
foo()
#> [1] "missing"
foo("")
#> [1] "fallback"
```
