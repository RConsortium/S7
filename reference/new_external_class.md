# Classes in other packages

An external class is a lightweight placeholder for an S7 class defined
in another package (or in your own package and needed before it's fully
defined). It carries only the package and class name, and is resolved to
the real S7 class when needed.

External classes are useful in two situations:

- To register a method for a generic in your package, dispatching on a
  class from a soft dependency. The method will be registered when `pkg`
  is loaded (using the same machinery as
  [`new_external_generic()`](https://rconsortium.github.io/S7/reference/new_external_generic.md)).

      SomeClass <- new_external_class("pkg", "SomeClass")
      method(my_generic, SomeClass) <- ...

- To refer to a class that hasn't been defined yet, such as a
  self-referential or mutually recursive class.

      tree_stub <- new_external_class("mypkg", "tree")
      new_class("tree", properties = list(child = NULL | tree_stub))

Make sure to call
[`S7_on_load()`](https://rconsortium.github.io/S7/reference/S7_on_load.md)
in your package's `.onLoad()` so that deferred method registrations fire
when the relevant package is loaded.

External classes can not currently be used as parents in
[`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md).
We hope to relax that restriction in the near future.

## Usage

``` r
new_external_class(package, name, version = NULL)
```

## Arguments

- package:

  Package the class is defined in.

- name:

  Name of the class, as a string.

- version:

  An optional version the package must meet for the method to be
  registered.

## Value

An S7 external class, i.e. a list with S3 class `S7_external_class`.

## Examples

``` r
# Refer to an S7 class in another package without taking a hard dependency:
TheirClass <- new_external_class("theirpkg", "TheirClass")
TheirClass
#> <S7_external_class> theirpkg::TheirClass

# Self-referential class: the `child` property can be another `tree`,
# or `NULL` to terminate the chain.
tree_stub <- new_external_class("mypkg", "tree")
tree <- new_class(
  name = "tree",
  package = "mypkg",
  properties = list(child = NULL | tree_stub)
)
```
