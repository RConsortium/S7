
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Object-oriented Programming Working Group

  - [Initial proposal](proposal/proposal.org)
  - [Requirements brainstorming](spec/requirements.md)
  - [Minutes](minutes/)
  - [Code](R/) (this repository is an R package)

<!-- badges: start -->

[![R-CMD-check](https://github.com/RConsortium/OOP-WG/workflows/R-CMD-check/badge.svg)](https://github.com/RConsortium/OOP-WG/actions)
[![Codecov test
coverage](https://codecov.io/gh/RConsortium/OOP-WG/branch/master/graph/badge.svg)](https://codecov.io/gh/RConsortium/OOP-WG?branch=master)
<!-- badges: end -->

## Workflow

  - File an issue to discuss the topic and build consensus.
  - Once consensus has been reached, the issue author should create a
    pull request that summarises the discussion in the appropriate `.md`
    file, and request review from all folks who participated the issue
    discussion.
  - Once all participants have accepted the PR, the original author
    merges.

## Example

``` r
library(R7)
#> 
#> Attaching package: 'R7'
#> The following object is masked from 'package:base':
#> 
#>     @

range <- class_new("range",
  constructor = function(start, end) {
    object_new(start = start, end = end)
  },
  validator = function(x) {
    if (property(x, "end") < property(x, "start")) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = c(start = "numeric", end = "numeric")
)

x <- range(start = 1, end = 10)

x@start
#> [1] 1

x@end
#> [1] 10

object_class(x)
#> r7: <range>
```

## Performance

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in the package there is some overhead due to
`.Call` vs `.Primitive`.

Dispatch currently uses a `.r7_methods` table defined in the environment
of the generic. This table is a nested set of hashed environments based
on the classes of the methods. e.g.

`method(foo, c("character", "numeric"))` method would be stored at

`.r7_methods[["foo"]][["character"]][["numeric"]]`

At each level the search iteratively searches up the class vector for
the object.

I am not sure caching is really needed, most objects have very short
class lists and the lookup is fast as each level should have relatively
few items and we are using hashed environments.

``` r
text <- class_new("text", parent = "character", constructor = function(text) object_new(.data = text))
number <- class_new("number", parent = "numeric", constructor = function(x) object_new(.data = x))

x <- text("hi")
y <- number(1)

foo_r7 <- generic_new(name = "foo_r7", signature = "x")
method(foo_r7, "text") <- function(x) paste0(x, "-foo")

foo_s3 <- function(x) {
  UseMethod("foo_s3")
}

foo_s3.text <- function(x) {
  paste0(x, "-foo")
}

library(methods)
setOldClass(c("number", "numeric", "r7_object"))
setOldClass(c("text", "character", "r7_object"))

setGeneric("foo_s4", function(x) standardGeneric("foo_s4"))
#> [1] "foo_s4"
setMethod("foo_s4", c("text"), function(x) paste0(x, "-foo"))

# Measure performance of single dispatch
bench::mark(foo_r7(x), foo_s3(x), foo_s4(x))
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 foo_r7(x)    4.42µs   5.68µs   173452.    4.01KB     69.4
#> 2 foo_s3(x)    3.64µs   5.02µs   178866.        0B      0  
#> 3 foo_s4(x)    3.84µs   4.43µs   206440.        0B     20.6


bar_r7 <- generic_new("bar_r7", c("x", "y"))
method(bar_r7, list("text", "number")) <- function(x, y) paste0(x, "-", y, "-bar")

setGeneric("bar_s4", function(x, y) standardGeneric("bar_s4"))
#> [1] "bar_s4"
setMethod("bar_s4", c("text", "number"), function(x, y) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_r7(x, y), bar_s4(x, y))
#> # A tibble: 2 x 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_r7(x, y)  10.01µs   10.9µs    85402.        0B    17.1 
#> 2 bar_s4(x, y)   9.63µs   10.3µs    90730.        0B     9.07
```

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
              - [ ] - It does *not* identify the class.
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
          - [ ] - Validates properties then adds to prototype.
          - [x] - Validates the complete object.
  - Shortcuts
      - [ ] - any argument that takes a class object can instead take
        the name of a class object as a string
      - [ ] - instead of providing a list of property objects, you can
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
      - [x] - An optional accessor function
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
          - [ ] - method\<- is designed to work at run-time
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
          - [ ] - `...` is not used for dispatch
  - Compatibility
      - S3
          - [x] - Since the class attribute has the same semantics as
            S3, S3 dispatch should be fully compatible.
          - [ ] - The new generics should also be able to handle legacy
            S3 objects.
      - S4
          - [x] - Since the new generics will fallback to S3 dispatch,
            they should support S4 objects just as S3 generics support
            them now.
  - Documentation
      - [ ] - Generate index pages that list the methods for a generic
        or the methods with a particular class in their signature

## Questions

  - Returning NULL / character in validator vs throwing an error?
