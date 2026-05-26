# Retrieve the class specification of an object

`S7_class()` returns a class specification for any R object, in a form
that can be passed to
[`method()`](https://rconsortium.github.io/S7/reference/method.md) or
used in any S7 dispatch context.

## Usage

``` r
S7_class(object)
```

## Arguments

- object:

  Any R object.

## Value

A class specification.

## Details

- For S7 objects, returns the [S7
  class](https://rconsortium.github.io/S7/reference/new_class.md).

- For S4 objects, returns the S4 class representation.

- For S3 objects, returns a
  [`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md)
  wrapping `class(x)`.

- For base types, returns the matching `class_*` (e.g.
  [class_integer](https://rconsortium.github.io/S7/reference/base_classes.md)
  for integer vectors,
  [class_function](https://rconsortium.github.io/S7/reference/base_classes.md)
  for functions).

- For `NULL`, returns `NULL`.

- For missing arguments, returns
  [class_missing](https://rconsortium.github.io/S7/reference/class_missing.md).

## Examples

``` r
Foo <- new_class("Foo")
S7_class(Foo())
#> <Foo> class
#> @ parent     : <S7_object>
#> @ constructor: function() {...}
#> @ validator  : <NULL>
#> @ properties :

# Also works on non-S7 objects
S7_class(1L)
#> <S7_base_class>: <integer>
S7_class("x")
#> <S7_base_class>: <character>
S7_class(mean)
#> <S7_base_class>: <function>
S7_class(factor("a"))
#> <S7_S3_class>: S3<factor>
```
