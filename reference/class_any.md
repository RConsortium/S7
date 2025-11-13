# Dispatch on any class

Use `class_any` to register a default method that is called when no
other methods are matched.

## Usage

``` r
class_any
```

## Examples

``` r
foo <- new_generic("foo", "x")
method(foo, class_numeric) <- function(x) "number"
method(foo, class_any) <- function(x) "fallback"

foo(1)
#> [1] "number"
foo("x")
#> [1] "fallback"
```
