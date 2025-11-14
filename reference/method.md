# Find a method for an S7 generic

`method()` takes a generic and class signature and performs method
dispatch to find the corresponding method implementation. This is rarely
needed because you'll usually rely on the the generic to do dispatch for
you (via
[`S7_dispatch()`](https://rconsortium.github.io/S7/reference/new_generic.md)).
However, this introspection is useful if you want to see the
implementation of a specific method.

## Usage

``` r
method(generic, class = NULL, object = NULL)
```

## Arguments

- generic:

  A generic function, i.e. an [S7
  generic](https://rconsortium.github.io/S7/reference/new_generic.md),
  an [external
  generic](https://rconsortium.github.io/S7/reference/new_external_generic.md),
  an [S3 generic](https://rdrr.io/r/base/UseMethod.html), or an [S4
  generic](https://rdrr.io/r/methods/setGeneric.html).

- class, object:

  Perform introspection either with a `class` (processed with
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md))
  or a concrete `object`. If `generic` uses multiple dispatch then both
  `object` and `class` must be a list of classes/objects.

## Value

Either a function with class `S7_method` or an error if no matching
method is found.

## See also

[`method_explain()`](https://rconsortium.github.io/S7/reference/method_explain.md)
to explain why a specific method was picked.

## Examples

``` r
# Create a generic and register some methods
bizarro <- new_generic("bizarro", "x")
method(bizarro, class_numeric) <- function(x) rev(x)
method(bizarro, class_factor) <- function(x) {
  levels(x) <- rev(levels(x))
  x
}

# Printing the generic shows the registered method
bizarro
#> <S7_generic> bizarro(x, ...) with 3 methods:
#> 1: method(bizarro, class_integer)
#> 2: method(bizarro, class_double)
#> 3: method(bizarro, new_S3_class("factor"))

# And you can use method() to inspect specific implementations
method(bizarro, class = class_integer)
#> <S7_method> method(bizarro, class_integer)
#> function (x) 
#> rev(x)
#> <environment: 0x561defade478>
method(bizarro, object = 1)
#> <S7_method> method(bizarro, class_double)
#> function (x) 
#> rev(x)
#> <environment: 0x561defade478>
method(bizarro, class = class_factor)
#> <S7_method> method(bizarro, new_S3_class("factor"))
#> function (x) 
#> {
#>     levels(x) <- rev(levels(x))
#>     x
#> }
#> <environment: 0x561defade478>

# errors if method not found
try(method(bizarro, class = class_data.frame))
#> Error : Can't find method for `bizarro(S3<data.frame>)`.
try(method(bizarro, object = "x"))
#> Error : Can't find method for `bizarro(<character>)`.
```
