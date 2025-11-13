# Register an S7 method for a generic

A generic defines the interface of a function. Once you have created a
generic with
[`new_generic()`](https://rconsortium.github.io/S7/reference/new_generic.md),
you provide implementations for specific signatures by registering
methods with `method<-`.

The goal is for `method<-` to be the single function you need when
working with S7 generics or S7 classes. This means that as well as
registering methods for S7 classes on S7 generics, you can also register
methods for S7 classes on S3 or S4 generics, and S3 or S4 classes on S7
generics. But this is not a general method registration function: at
least one of `generic` and `signature` needs to be from S7.

Note that if you are writing a package, you must call
[`methods_register()`](https://rconsortium.github.io/S7/reference/methods_register.md)
in your `.onLoad`. This ensures that all methods are dynamically
registered when needed.

## Usage

``` r
method(generic, signature) <- value
```

## Arguments

- generic:

  A generic function, i.e. an [S7
  generic](https://rconsortium.github.io/S7/reference/new_generic.md),
  an [external
  generic](https://rconsortium.github.io/S7/reference/new_external_generic.md),
  an [S3 generic](https://rdrr.io/r/base/UseMethod.html), or an [S4
  generic](https://rdrr.io/r/methods/setGeneric.html).

- signature:

  A method signature.

  For S7 generics that use single dispatch, this must be one of the
  following:

  - An S7 class (created by
    [`new_class()`](https://rconsortium.github.io/S7/reference/new_class.md)).

  - An S7 union (created by
    [`new_union()`](https://rconsortium.github.io/S7/reference/new_union.md)).

  - An S3 class (created by
    [`new_S3_class()`](https://rconsortium.github.io/S7/reference/new_S3_class.md)).

  - An S4 class (created by
    [`methods::getClass()`](https://rdrr.io/r/methods/getClass.html) or
    [`methods::new()`](https://rdrr.io/r/methods/new.html)).

  - A base type like
    [class_logical](https://rconsortium.github.io/S7/reference/base_classes.md),
    [class_integer](https://rconsortium.github.io/S7/reference/base_classes.md),
    or
    [class_numeric](https://rconsortium.github.io/S7/reference/base_classes.md).

  - A special type like
    [class_missing](https://rconsortium.github.io/S7/reference/class_missing.md)
    or
    [class_any](https://rconsortium.github.io/S7/reference/class_any.md).

  For S7 generics that use multiple dispatch, this must be a list of any
  of the above types.

  For S3 generics, this must be a single S7 class.

  For S4 generics, this must either be an S7 class, or a list that
  includes at least one S7 class.

- value:

  A function that implements the generic specification for the given
  `signature`.

## Value

The `generic`, invisibly.

## Examples

``` r
# Create a generic
bizarro <- new_generic("bizarro", "x")
# Register some methods
method(bizarro, class_numeric) <- function(x) rev(x)
method(bizarro, new_S3_class("data.frame")) <- function(x) {
  x[] <- lapply(x, bizarro)
  rev(x)
}

# Using a generic calls the methods automatically
bizarro(head(mtcars))
#>                   carb gear am vs  qsec    wt drat  hp disp cyl  mpg
#> Mazda RX4            1    3  0  1 20.22 3.460 2.76 105  225   6 18.1
#> Mazda RX4 Wag        2    3  0  0 17.02 3.440 3.15 175  360   8 18.7
#> Datsun 710           1    3  0  1 19.44 3.215 3.08 110  258   6 21.4
#> Hornet 4 Drive       1    4  1  1 18.61 2.320 3.85  93  108   4 22.8
#> Hornet Sportabout    4    4  1  0 17.02 2.875 3.90 110  160   6 21.0
#> Valiant              4    4  1  0 16.46 2.620 3.90 110  160   6 21.0
```
