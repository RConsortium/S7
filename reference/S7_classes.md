# Find S7 classes and generics in an environment

- `S7_classes()` returns the names of S7 classes defined in `env`.

- `S7_generics()` returns the names of S7 generics defined in `env`.

## Usage

``` r
S7_classes(env = parent.frame())

S7_generics(env = parent.frame())
```

## Arguments

- env:

  An environment. Defaults to the caller's environment. To inspect a
  package, pass `asNamespace("pkg")`; to inspect the global environment,
  pass [`globalenv()`](https://rdrr.io/r/base/environment.html).

## Value

A character vector of names.

## Examples

``` r
# List S7 classes exported by the S7 package itself
S7_classes(asNamespace("S7"))
#> [1] "S7_object"
S7_generics(asNamespace("S7"))
#> [1] "convert"
```
