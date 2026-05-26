# Format a class specification as a string

`S7_class_desc()` turns any [class
specification](https://rconsortium.github.io/S7/reference/as_class.md)
into a short, human-readable, string, suitable for use in user-facing
messages.

## Usage

``` r
S7_class_desc(class)
```

## Arguments

- class:

  A [class
  specification](https://rconsortium.github.io/S7/reference/as_class.md).

## Value

A string.

## Examples

``` r
S7_class_desc(class_integer)
#> [1] "<integer>"
S7_class_desc(new_S3_class("data.frame"))
#> [1] "S3<data.frame>"
S7_class_desc(class_integer | class_double)
#> [1] "<integer> or <double>"
S7_class_desc(NULL)
#> [1] "<NULL>"
```
