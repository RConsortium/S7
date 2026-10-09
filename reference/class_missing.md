# Dispatch on a missing argument

Use `class_missing` to dispatch when no argument value is available.
Omitted arguments with defaults dispatch on the default's class. Missing
values detected by [`is.na()`](https://rdrr.io/r/base/NA.html) do not
trigger `class_missing` dispatch.

## Usage

``` r
class_missing
```

## Value

Sentinel objects used for special types of dispatch.

## Examples

``` r
foo := new_generic("x")
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
