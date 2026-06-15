# List S7 methods

List the methods registered on an S7 `generic`, or the methods
registered for a given `class` across all S7 generics defined in
attached packages.

## Usage

``` r
S7_methods(generic = NULL, class = NULL)
```

## Arguments

- generic:

  An S7 generic.

- class:

  A class specification (anything accepted by
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md)).
  When supplied, every S7 generic in every attached package is searched
  for methods with this class in their signature.

## Value

A data frame with one row per matching method and columns:

- `generic`: the generic's name.

- `package`: the package the generic is defined in, or `NA` for generics
  found in the global environment (or when `generic` is supplied
  directly).

- `signature`: a list column of `S7_signature` objects describing the
  dispatch signature. [`format()`](https://rdrr.io/r/base/format.html)
  them for a human-readable description.

## Examples

``` r
Foo <- new_class("Foo", package = NULL)
Bar <- new_class("Bar", package = NULL)
my_gen <- new_generic("my_gen", "x")
method(my_gen, Foo) <- function(x) "foo"
method(my_gen, Bar) <- function(x) "bar"

S7_methods(generic = my_gen)
#>   generic package signature
#> 1  my_gen    <NA>     <Foo>
#> 2  my_gen    <NA>     <Bar>
S7_methods(class = Foo)
#> [1] generic   package   signature
#> <0 rows> (or 0-length row.names)
```
