# Property introspection

- `prop_names(x)` returns the names of the properties

- `prop_exists(x, "prop")` returns `TRUE` iif `x` has property `prop`.

## Usage

``` r
prop_names(object)

prop_exists(object, name)
```

## Arguments

- object:

  An object from a S7 class

- name:

  The name of the parameter as a character. Partial matching is not
  performed.

## Value

`prop_names()` returns a character vector; `prop_exists()` returns a
single `TRUE` or `FALSE`.

## Examples

``` r
Foo <- new_class("Foo", properties = list(a = class_character, b = class_integer))
f <- Foo()

prop_names(f)
#> [1] "a" "b"
prop_exists(f, "a")
#> [1] TRUE
prop_exists(f, "c")
#> [1] FALSE
```
