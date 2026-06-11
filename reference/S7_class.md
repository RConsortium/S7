# Retrieve the class specification of an object

`S7_class()` returns a [class
specification](https://rconsortium.github.io/S7/reference/as_class.md)
for any R object, in a form that can be passed to
[`method()`](https://rconsortium.github.io/S7/reference/method.md) or
used in any S7 dispatch context.

- For S7 objects, the [S7
  class](https://rconsortium.github.io/S7/reference/new_class.md).

- For S3 objects, a
  [`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md)
  wrapping `class(x)`.

- For S4 objects, the S4 class.

- For base types, the matching `class_*` (e.g.
  [class_integer](https://rconsortium.github.io/S7/reference/base_classes.md)).

- For missing arguments, returns
  [class_missing](https://rconsortium.github.io/S7/reference/class_missing.md).

## Usage

``` r
S7_class(object)
```

## Arguments

- object:

  Any R object.

## Value

A class specification.

## Examples

``` r
Foo := new_class()
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
