# Register an S7, S3, or union class with S4

If you want to use method\<- to register a method for an S4 generic with
an S7 class that does not extend an S4 class, or an S3 class created by
[`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md),
you need to call `S4_register()` once. Classes created by
[`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)
with an S4 parent are registered automatically.

## Usage

``` r
S4_register(class, env = parent.frame())

S4_contains(class, env = parent.frame())
```

## Arguments

- class:

  An S7 class created with
  [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md),
  or, for `S4_register()` only, an S3 class created with
  [`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md)
  or an S7 union created with
  [`new_union()`](https://rconsortium.github.io/S7/reference/new_union.md).

- env:

  Expert use only. Environment where S4 class will be registered.

## Value

Called for its side effects and invisibly returns the registered S4
class name.

## Details

`S4_register()` registers an S7 class, S3 class, or S7 union with S4 and
invisibly returns the registered S4 class name. For S7 classes, this
creates a virtual S4 old class that exposes stored S7 properties as S4
slots and carries the `_S7_class` slot needed for S7 dispatch and
validation.

After registration, `S4_contains()` returns the virtual S4 class name to
use when an S4 class should extend an S7 class with
`methods::setClass(contains = )`. It asserts that the S7 properties can
safely cross the S4 inheritance boundary.

Register S7 unions with `S4_register()` before using them in S4 slots or
method signatures unless an equivalent S4 union already exists.

See
[`vignette("compatibility")`](https://rconsortium.github.io/S7/articles/compatibility.md)
for examples and caveats.

## Examples

``` r
methods::setGeneric("S4_generic", function(x) {
  standardGeneric("S4_generic")
})
#> [1] "S4_generic"

Foo := new_class()
S4_register(Foo)
method(S4_generic, Foo) <- function(x) "Hello"

S4_generic(Foo())
#> [1] "Hello"

S4Foo := new_class(properties = list(x = class_numeric), package = "S7")
S4_register(S4Foo)
S4Foo_S4 <- S4_contains(S4Foo)
methods::setClass("S4Child", contains = S4Foo_S4)
```
