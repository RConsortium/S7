# Generics in other packages

You need an explicit external generic when you want to provide methods
for a generic (S3, S4, or S7) that is defined in another package, and
you don't want to take a hard dependency on that package.

The easiest way to provide methods for generics in other packages is
import the generic into your `NAMESPACE`. This, however, creates a hard
dependency, and sometimes you want a soft dependency, only registering
the method if the package is already installed. `new_external_generic()`
allows you to provide the minimal needed information about a generic so
that methods can be registered at run time, as needed, using
[`methods_register()`](https://rconsortium.github.io/S7/reference/methods_register.md).

Note that in tests, you'll need to explicitly call the generic from the
external package with `pkg::generic()`.

## Usage

``` r
new_external_generic(package, name, dispatch_args, version = NULL)
```

## Arguments

- package:

  Package the generic is defined in.

- name:

  Name of generic, as a string.

- dispatch_args:

  Character vector giving arguments used for dispatch.

- version:

  An optional version the package must meet for the method to be
  registered.

## Value

An S7 external generic, i.e. a list with class `S7_external_generic`.

## Examples

``` r
MyClass <- new_class("MyClass")

your_generic <- new_external_generic("stats", "median", "x")
method(your_generic, MyClass) <- function(x) "Hi!"
```
