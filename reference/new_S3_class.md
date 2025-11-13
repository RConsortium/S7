# Declare an S3 class

To use an S3 class with S7, you must explicitly declare it using
`new_S3_class()` because S3 lacks a formal class definition. (Unless
it's an important base class already defined in
[base_s3_classes](https://rconsortium.github.io/S7/reference/base_s3_classes.md).)

## Usage

``` r
new_S3_class(class, constructor = NULL, validator = NULL)
```

## Arguments

- class:

  S3 class vector (i.e. what
  [`class()`](https://rdrr.io/r/base/class.html) returns). For method
  registration, you can abbreviate this to a single string, the S3 class
  name.

- constructor:

  An optional constructor that can be used to create objects of the
  specified class. This is only needed if you wish to have an S7 class
  inherit from an S3 class or to use the S3 class as a property without
  a default. It must be specified in the same way as a S7 constructor:
  the first argument should be `.data` (the base type whose attributes
  will be modified).

  All arguments to the constructor should have default values so that
  when the constructor is called with no arguments, it returns returns
  an "empty", but valid, object.

- validator:

  An optional validator used by
  [`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
  to check that the S7 object adheres to the constraints of the S3
  class.

  A validator is a single argument function that takes the object to
  validate and returns `NULL` if the object is valid. If the object is
  invalid, it returns a character vector of problems.

## Value

An S7 definition of an S3 class, i.e. a list with class `S7_S3_class`.

## Method dispatch, properties, and unions

There are three ways of using S3 with S7 that only require the S3 class
vector:

- Registering a S3 method for an S7 generic.

- Restricting an S7 property to an S3 class.

- Using an S3 class in an S7 union.

This is easy, and you can usually include the `new_S3_class()` call
inline:

    method(my_generic, new_S3_class("factor")) <- function(x) "A factor"
    new_class("MyClass", properties = list(types = new_S3_class("factor")))
    new_union("character", new_S3_class("factor"))

## Extending an S3 class

Creating an S7 class that extends an S3 class requires more work. You'll
also need to provide a constructor for the S3 class that follows S7
conventions. This means the first argument to the constructor should be
`.data`, and it should be followed by one argument for each attribute
used by the class.

This can be awkward because base S3 classes are usually heavily wrapped
for user convenience and no low level constructor is available. For
example, the factor class is an integer vector with a character vector
of `levels`, but there's no base R function that takes an integer vector
of values and character vector of levels, verifies that they are
consistent, then creates a factor object.

You may optionally want to also provide a `validator` function which
will ensure that
[`validate()`](https://rconsortium.github.io/S7/reference/validate.md)
confirms the validity of any S7 classes that build on this class. Unlike
an S7 validator, you are responsible for validating the types of the
attributes.

The following code shows how you might wrap the base Date class. A Date
is a numeric vector with class `Date` that can be constructed with
[`.Date()`](https://rdrr.io/r/base/base-internal.html).

    S3_Date <- new_S3_class("Date",
      function(.data = integer()) {
        .Date(.data)
      },
      function(self) {
        if (!is.numeric(self)) {
          "Underlying data must be numeric"
        }
      }
    )

## Examples

``` r
# No checking, just used for dispatch
Date <- new_S3_class("Date")

my_generic <- new_generic("my_generic", "x")
method(my_generic, Date) <- function(x) "This is a date"

my_generic(Sys.Date())
#> [1] "This is a date"
```
