# Explain method dispatch

`method_explain()` shows all possible methods that a call to a generic
might use, which ones exist, and which one will actually be called.

Note that method dispatch uses a string representation of each class in
the class hierarchy. Each class system uses a slightly different
convention to avoid ambiguity.

- S7: `pkg::class` or `class`

- S4: `S4/pkg::class` or `S4/class`

- S3: `class`

## Usage

``` r
method_explain(generic, class = NULL, object = NULL)
```

## Arguments

- generic:

  An S7 generic, i.e. the result of
  [`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md).
  Unlike method\<-,
  [`method()`](https://rconsortium.github.io/S7/reference/method.md)
  only works with S7 generics; it does not look up methods registered on
  S3 or S4 generics.

- class, object:

  Perform introspection either with a `class` (processed with
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md))
  or a concrete `object`. If `generic` uses multiple dispatch then both
  `object` and `class` must be a list of classes/objects.

## Value

Nothing; this function is called for it's side effects.

## Examples

``` r
Foo1 <- new_class("Foo1")
Foo2 <- new_class("Foo2", Foo1)

add <- new_generic("add", c("x", "y"))
method(add, list(Foo2, Foo1)) <- function(x, y) c(2, 1)
method(add, list(Foo1, Foo1)) <- function(x, y) c(1, 1)

method_explain(add, list(Foo2, Foo2))
#>    add([Foo2], [Foo2])
#> -> add([Foo2], [Foo1])
#>    add([Foo2], [S7_object])
#>    add([Foo2], [ANY])
#>    add([Foo1], [Foo2])
#> *  add([Foo1], [Foo1])
#>    add([Foo1], [S7_object])
#>    add([Foo1], [ANY])
#>    add([S7_object], [Foo2])
#>    add([S7_object], [Foo1])
#>    add([S7_object], [S7_object])
#>    add([S7_object], [ANY])
#>    add([ANY], [Foo2])
#>    add([ANY], [Foo1])
#>    add([ANY], [S7_object])
#>    add([ANY], [ANY])
```
