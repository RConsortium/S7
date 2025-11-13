# Does this object inherit from an S7 class?

- `S7_inherits()` returns `TRUE` or `FALSE`.

- `check_is_S7()` throws an error if `x` isn't the specified `class`.

## Usage

``` r
S7_inherits(x, class = NULL)

check_is_S7(x, class = NULL, arg = deparse(substitute(x)))
```

## Arguments

- x:

  An object

- class:

  An S7 class or `NULL`. If `NULL`, tests whether `x` is an S7 object
  without testing for a specific class.

- arg:

  Argument name used in error message.

## Value

- `S7_inherits()` returns a single `TRUE` or `FALSE`.

- `check_is_S7()` returns nothing; it's called for its side-effects.

## Note

Starting with R 4.3.0,
[`base::inherits()`](https://rdrr.io/r/base/class.html) can accept an S7
class as the second argument, supporting usage like `inherits(x, Foo)`.

## Examples

``` r
Foo1 <- new_class("Foo1")
Foo2 <- new_class("Foo2")

S7_inherits(Foo1(), Foo1)
#> [1] TRUE
check_is_S7(Foo1())
check_is_S7(Foo1(), Foo1)

S7_inherits(Foo1(), Foo2)
#> [1] FALSE
try(check_is_S7(Foo1(), Foo2))
#> Error : `Foo1()` must be a <Foo2>, not a <Foo1>

if (getRversion() >= "4.3.0")
  inherits(Foo1(), Foo1)
#> [1] TRUE
```
