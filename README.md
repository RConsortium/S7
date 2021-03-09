
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Object-oriented Programming Working Group

  - [Initial proposal](proposal/proposal.org)
  - [Requirements brainstorming](spec/requirements.md)
  - [Minutes](minutes/)
  - [Code](R/) (this repository is an R package)

These ideas have been implemented in the R7 package, hosted in this
repository.

<!-- badges: start -->

[![R-CMD-check](https://github.com/RConsortium/OOP-WG/workflows/R-CMD-check/badge.svg)](https://github.com/RConsortium/OOP-WG/actions)
[![Codecov test
coverage](https://codecov.io/gh/RConsortium/OOP-WG/branch/master/graph/badge.svg)](https://codecov.io/gh/RConsortium/OOP-WG?branch=master)
<!-- badges: end -->

## Classes and objects

``` r
library(R7)

range <- class_new("range",
  constructor = function(start, end) {
    object_new(start = start, end = end)
  },
  validator = function(x) {
    if (x@end < x@start) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = list(
    start = "numeric",
    end = "numeric",
    property_new(
      name = "length",
      class = "numeric",
      getter = function(x) x@end - x@start,
      setter = function(x, value) {
        x@end <- x@start + value
        x
      }
    )
  )
)

x <- range(start = 1, end = 10)

x@start
#> [1] 1

x@end
#> [1] 10

x@length
#> [1] 9

x@length <- 5

x@length
#> [1] 5

# incorrect properties throws an error
x@middle
#> Error: Can't find property 'middle' in <range>

# assigning properties verifies the class matches the class of the value
x@end <- "foo"
#> Error: `value` must be of class <numeric>:
#> - `value` is of class <character>

# assigning properties runs the validator
x@end <- 0
#> Error: Invalid <range> object:
#> - `end` must be greater than or equal to `start`

# Print methods for both R7_class objects
object_class(x)
#> <R7_class>
#> @name range
#> @parent <R7_object>
#> @properties
#>  $start  <numeric>
#>  $end    <numeric>
#>  $length <numeric>

# As well as normal R7_objects
x
#> <R7_object> <range>
#> @start  1
#> @end    6
#> @length 5

# Use `.data` to refer to and retrieve the base data type, properties are
# automatically removed, but non-property attributes (such as names) are retained.

text <- class_new("text", parent = "character", constructor = function(text) object_new(.data = text))

y <- text(c(foo = "bar"))

str(y@.data)
#>  Named chr "bar"
#>  - attr(*, "names")= chr "foo"
```

## Generics and methods

``` r
text <- class_new("text", parent = "character", constructor = function(text) object_new(.data = text))

foo <- generic_new(name = "foo", signature = "x")

method_new(foo, list("text"), function(x) paste0("foo-", x))

foo(text("hi"))
#> [1] "foo-hi"
```

## Multiple dispatch

``` r
number <- class_new("number", parent = "numeric", constructor = function(x) object_new(.data = x))

bar <- generic_new(name = "bar", signature = c("x", "y"))

method_new(bar, list("character", "numeric"), function(x, y) paste0("foo-", x, ":", y))

bar(text("hi"), number(42))
#> [1] "foo-hi:42"
```

## Calling the next method

``` r
method_new(bar, list("text", "number"), function(x, y) {
  res <- method_next(bar, list(object_class(x), object_class(y)))(x, y)
  paste0("2 ", res)
})

bar(text("hi"), number(42))
#> [1] "2 foo-hi:42"
```

### Load time registration

``` r
.onLoad <- function(libname, pkgname) {
  R7::method_register()
}

method_new("pkg1::foo", list("text", "numeric"), function(x, y) paste0("foo-", x, ": ", y))
```

## Performance

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in the package there is some overhead due to
`.Call` vs `.Primitive`.

Dispatch uses a table stored in the `methods` property of the generic.
This table is a nested set of hashed environments based on the classes
of the methods. e.g.

`method(foo, c("character", "numeric"))` method would be stored at

`foo@methods[["character"]][["numeric"]]`

At each level the search iteratively searches up the class vector for
the object.

A potential optimization is caching based on the class names, but lookup
should be fast even without this.

``` r
text <- class_new("text", parent = "character", constructor = function(text) object_new(.data = text))
number <- class_new("number", parent = "numeric", constructor = function(x) object_new(.data = x))

x <- text("hi")
y <- number(1)

foo_R7 <- generic_new(name = "foo_R7", signature = "x")
method(foo_R7, "text") <- function(x) paste0(x, "-foo")

foo_s3 <- function(x) {
  UseMethod("foo_s3")
}

foo_s3.text <- function(x) {
  paste0(x, "-foo")
}

library(methods)
setOldClass(c("number", "numeric", "R7_object"))
setOldClass(c("text", "character", "R7_object"))

setGeneric("foo_s4", function(x) standardGeneric("foo_s4"))
#> [1] "foo_s4"
setMethod("foo_s4", c("text"), function(x) paste0(x, "-foo"))

# Measure performance of single dispatch
bench::mark(foo_R7(x), foo_s3(x), foo_s4(x))
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 foo_R7(x)    5.74µs   8.62µs   117261.        0B     11.7
#> 2 foo_s3(x)    3.83µs   4.19µs   217581.        0B     21.8
#> 3 foo_s4(x)       4µs    4.4µs   216754.        0B     21.7


bar_R7 <- generic_new("bar_R7", c("x", "y"))
method(bar_R7, list("text", "number")) <- function(x, y) paste0(x, "-", y, "-bar")

setGeneric("bar_s4", function(x, y) standardGeneric("bar_s4"))
#> [1] "bar_s4"
setMethod("bar_s4", c("text", "number"), function(x, y) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_R7(x, y), bar_s4(x, y))
#> # A tibble: 2 x 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_R7(x, y)  11.62µs   12.7µs    74092.        0B     22.2
#> 2 bar_s4(x, y)   9.79µs   10.8µs    79660.        0B     15.9
```

## Questions

  - Best way to support `substitute()` calls in methods? We need to
    evaluate the argument promises to do the dispatch, but we want to
    pass the un-evaluated promise to the call?
  - Should we remove `method()<-`, We can’t do the following, really
    want to keep this syntax if possible.
      - can’t use `method("foo")<-`
      - can’t use `method(otherpkg::foo)<-`
      - can’t use `method("otherpkg::foo")<-`
  - What should happen if you call `method_new()` on a S3 generic?
    1.  Should we create a new R7 generic out of the S3 generic?
    2.  Or just register the R7 object using `registerS3method()`? ++

## Design workflow

  - File an issue to discuss the topic and build consensus.
  - Once consensus has been reached, the issue author should create a
    pull request that summarises the discussion in the appropriate `.md`
    file, and request review from all folks who participated the issue
    discussion.
  - Once all participants have accepted the PR, the original author
    merges.

## TODO

  - Objects
      - [x] - A class object attribute, a reference to the class object,
        and retrieved with `object_class()`.
      - [x] - For S3 compatibility, a class attribute, a character
        vector of class names.
      - [x] - Additional attributes storing properties defined by the
        class, accessible with `@/property()`.
  - Classes
      - [x] - R7 classes are first class objects with the following
          - [x] - `name`, a human-meaningful descriptor for the class.
          - [x] - `parent`, the class object of the parent class.
          - [x] - A constructor, an user-facing function used to create
            new objects of this class. It always ends with a call to
            `object_new()` to initialize the class.
          - [x] - A validator, a function that takes the object and
            returns NULL if the object is valid, otherwise a character
            vector of error messages.
          - [x] - properties, a list of property objects
  - Initialization
      - [x] - The constructor uses `object_new()` to initialize a new
        object, this
          - [x] - Inspects the enclosing scope to find the “current”
            class.
          - [ ] - Creates the prototype, by either by calling the parent
            constructor or by creating a base type and adding class and
            `object_class()` attributes to it.
          - [x] - Validates properties then adds to prototype.
          - [x] - Validates the complete object.
  - Shortcuts
      - [ ] - any argument that takes a class object can instead take
        the name of a class object as a string
      - [x] - instead of providing a list of property objects, you can
        instead provide a named character vector.
  - Validation
      - [x] - valid\_eventually
      - [x] - valid\_implicitly
  - Unions
      - [ ] - Used in properties to allow a property to be one of a set
        of classes
      - [x] - In method dispatch as a convenience for defining a method
        for multiple classes
  - Properties
      - [x] - Accessed using `property()` / `property<-`
      - [x] - Accessed using `@` / `@<-`
      - [x] - A name, used to label output
      - [ ] - A optional class or union
      - [x] - An optional accessor functions, both getter and setters
      - [ ] - Properties are created with `prop_new()`
  - Generics
      - [x] - It knows its name and the names of the arguments in its
        signature
      - [x] - Calling `generic_new()` defines a new generic
      - [ ] - By convention, any argument that takes a generic function,
        can instead take the name of a generic function supplied as a
        string
  - Methods
      - Registration
          - [x] - Methods are defined by calling method\<-(generic,
            signature, method):
          - [x] - generic is a generic function.
          - [x] - signature is a
              - [x] - single class object
              - [x] - a class union
              - [x] - list of class objects/unions
              - [x] - a character vector.
          - [ ] - method is a compatible function
          - [x] - `method_new` is designed to work at run-time
              - [ ] - `method_new` should optionally take a package
                version, so the method is only registered if the package
                is newer than the version.
          - [ ] - Can define methods where one of the arguments is
            missing
          - [ ] - Can define methods where one of the arguments has any
            type
      - Dispatch
          - [x] - Dispatch is nested, meaning that if there are multiple
            arguments in the generic signature, it will dispatch on the
            first argument, then the second.
          - [x] - A `plot()` generic dispatching on `x`, e.g. `plot <-
            function(x) { method(plot, object_class(x))(x) }`
          - [x] - A `publish()` that publishes an object `x` to a
            destination `y`, dispatching on both arguments,
            e.g. `publish <- function(x, y, ...) { method(publish,
            list(object_class(x), object_class(y)))(x, y, ...) }`
          - [x] - `...` is not used for dispatch
          - [x] - R7 generics can dispatch with base type objects
          - [x] - R7 generics can dispatch with S3 objects
          - [x] - R7 generics can dispatch with S4 objects
          - [x] - `method_next()` can dispatch on multiple arguments,
            avoiding methods that have already been called.
  - Compatibility
      - S3
          - [x] - Since the class attribute has the same semantics as
            S3, S3 dispatch should be fully compatible.
          - [x] - The new generics should also be able to handle legacy
            S3 objects.
          - [x] - `method()` falls back to single argument S3 dispatch
            if the R7 dispatch fails.
          - [ ] - `method()` uses S3 group generics as well
      - S4
          - [x] - Since the new generics will fallback to S3 dispatch,
            they should support S4 objects just as S3 generics support
            them now.
  - Documentation
      - [ ] - Generate index pages that list the methods for a generic
        or the methods with a particular class in their signature
