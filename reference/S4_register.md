# Register an S7 class with S4

If you want to use method\<- to register an method for an S4 generic
with an S7 class, you need to call `S4_register()` once.

## Usage

``` r
S4_register(class, env = parent.frame())
```

## Arguments

- class:

  An S7 class created with
  [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md).

- env:

  Expert use only. Environment where S4 class will be registered.

## Value

Nothing; the function is called for its side-effect.

## Examples

``` r
methods::setGeneric("S4_generic", function(x) {
  standardGeneric("S4_generic")
})
#> [1] "S4_generic"

Foo <- new_class("Foo")
S4_register(Foo)
method(S4_generic, Foo) <- function(x) "Hello"

S4_generic(Foo())
#> [1] "Hello"
```
