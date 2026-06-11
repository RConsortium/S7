# Create and name an object in one step

Functions like
[`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
and
[`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md)
take a `name` that, by convention, matches the name of the variable that
you assign their result to. The `:=` operator eliminates this
duplication: `Foo := new_class()` is equivalent to
`Foo <- new_class(name = "Foo")`.

`:=` works with any function that has a `name` argument, but bear in
mind that it adds `name` to the call as a named argument, so any other
arguments supplied positionally will shift to fill the remaining
parameters.

## Usage

``` r
`:=`(lhs, rhs)
```

## Arguments

- lhs:

  A bare symbol: both the name of the variable to create and the `name`
  supplied to the right-hand side.

- rhs:

  A call to a function with a `name` argument.

## Value

The result of evaluating `rhs`, which is also assigned to `lhs` in the
calling environment, returned invisibly.

## Examples

``` r
Range := new_class(properties = list(
  start = class_double,
  end = class_double
))
Range
#> <Range> class
#> @ parent     : <S7_object>
#> @ constructor: function(start, end) {...}
#> @ validator  : <NULL>
#> @ properties :
#>  $ start: <double> = numeric(0)
#>  $ end: <double> = numeric(0)

describe := new_generic("x")
describe
#> <S7_generic> describe(x, ...) with 0 methods:
```
