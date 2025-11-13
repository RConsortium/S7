# Standard class specifications

This is used as the interface between S7 and R's other OO systems,
allowing you to use S7 classes and methods with base types, informal S3
classes, and formal S4 classes.

## Usage

``` r
as_class(x, arg = deparse(substitute(x)))
```

## Arguments

- x:

  A class specification. One of the following:

  - An S7 class (created by
    [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)).

  - An S7 union (created by
    [`new_union()`](https://rconsortium.github.io/S7/reference/new_union.md)).

  - An S3 class (created by
    [`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md)).

  - An S4 class (created by
    [`methods::getClass()`](https://rdrr.io/r/methods/getClass.html) or
    [`methods::new()`](https://rdrr.io/r/methods/new.html)).

  - A base class, like
    [class_logical](https://rconsortium.github.io/S7/reference/base_classes.md),
    [class_integer](https://rconsortium.github.io/S7/reference/base_classes.md),
    or
    [class_double](https://rconsortium.github.io/S7/reference/base_classes.md).

  - A "special", either
    [class_missing](https://rconsortium.github.io/S7/reference/class_missing.md)
    or
    [class_any](https://rconsortium.github.io/S7/reference/class_any.md).

- arg:

  Argument name used when generating errors.

## Value

A standardised class: either `NULL`, an S7 class, an S7 union, as
[new_S3_class](https://rconsortium.github.io/S7/reference/new_S3_class.md),
or a S4 class.

## Examples

``` r
as_class(class_logical)
#> <S7_base_class>: <logical>
as_class(new_S3_class("factor"))
#> <S7_S3_class>: S3<factor>
```
