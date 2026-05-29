# Does this object inherit from a class?

- `S7_inherits()` returns `TRUE` or `FALSE`.

- `check_is_S7()` throws an error if `x` isn't the specified `class`.

## Usage

``` r
S7_inherits(x, class = NULL)

check_is_S7(
  x,
  class = NULL,
  arg = deparse(substitute(x)),
  call = sys.call(-1L)
)
```

## Arguments

- x:

  An object.

- class:

  A class specification (anything accepted by
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md):
  an S7 class, S7 union, S3 class, S4 class, base type wrapper like
  [class_integer](https://rconsortium.github.io/S7/reference/base_classes.md),
  or
  [class_any](https://rconsortium.github.io/S7/reference/class_any.md)/[class_missing](https://rconsortium.github.io/S7/reference/class_missing.md)).
  If `NULL`, only tests whether `x` is an S7 object, without testing for
  a specific class.

- arg:

  Argument name used in error message.

- call:

  The call to report in the error message. Defaults to the calling
  function.

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
#> Error in try(check_is_S7(Foo1(), Foo2)) : 
#>   `Foo1()` must be a <Foo2>, not a <Foo1>.

# Also works with other class specifications
S7_inherits(1L, class_integer)
#> [1] TRUE
S7_inherits(data.frame(), new_S3_class("data.frame"))
#> [1] TRUE
S7_inherits(1L, class_integer | class_character)
#> [1] TRUE

if (getRversion() >= "4.3.0") {
  inherits(Foo1(), Foo1)
}
#> [1] TRUE
```
