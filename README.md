
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

range <- new_class("range",
  constructor = function(start, end) {
    new_object(start = start, end = end)
  },
  validator = function(x) {
    if (x@end < x@start) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = list(
    start = "numeric",
    end = "numeric",
    new_property(
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

text <- new_class("text", parent = "character", constructor = function(text) new_object(.data = text))

y <- text(c(foo = "bar"))

str(y@.data)
#>  Named chr "bar"
#>  - attr(*, "names")= chr "foo"
```

## Generics and methods

``` r
text <- new_class("text", parent = "character", constructor = function(text) new_object(.data = text))

foo <- new_generic(name = "foo", signature = "x")

method(foo, "text") <- function(x, ...) paste0("foo-", x)

foo(text("hi"))
#> [1] "foo-hi"
```

## Multiple dispatch

``` r
number <- new_class("number", parent = "numeric", constructor = function(x) new_object(.data = x))

bar <- new_generic(name = "bar", signature = c("x", "y"))

method(bar, list("character", "numeric")) <- function(x, y, ...) paste0("foo-", x, ":", y)

bar(text("hi"), number(42))
#> [1] "foo-hi:42"
```

## Calling the next method

``` r
method(bar, list("text", "number")) <- function(x, y, ...) {
  res <- next_method()(x, y)
  paste0("2 ", res)
}

bar(text("hi"), number(42))
#> [1] "2 foo-hi:42"
```

## Non-standard evaluation

``` r
subset2 <- new_generic(name = "subset", signature = "x")

method(subset2, "data.frame") <- function(x, subset = NULL, select = NULL, drop = FALSE, ...) {
  e <- substitute(subset)
  r <- eval(e, x, parent.frame())
  r <- r & !is.na(r)
  nl <- as.list(seq_along(x))
  names(nl) <- names(x)
  vars <- eval(substitute(select), nl, parent.frame())
  x[r, vars, drop = drop]
}

subset2(mtcars, hp > 200, c(wt, qsec))
#>                        wt  qsec
#> Duster 360          3.570 15.84
#> Cadillac Fleetwood  5.250 17.98
#> Lincoln Continental 5.424 17.82
#> Chrysler Imperial   5.345 17.42
#> Camaro Z28          3.840 15.41
#> Ford Pantera L      3.170 14.50
#> Maserati Bora       3.570 14.60
```

### Load time registration

``` r
.onLoad <- function(libname, pkgname) {
  R7::method_register()
}

foo <- new_external_method("pkg1", "foo", c("x", "y"))

method(foo, list("text", "numeric")) <- function(x, y, ...) paste0("foo-", x, ": ", y)
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

``` r
text <- new_class("text", parent = "character", constructor = function(text) new_object(.data = text))
number <- new_class("number", parent = "numeric", constructor = function(x) new_object(.data = x))

x <- text("hi")
y <- number(1)

foo_R7 <- new_generic(name = "foo_R7", signature = "x")
method(foo_R7, "text") <- function(x, ...) paste0(x, "-foo")

foo_s3 <- function(x, ...) {
  UseMethod("foo_s3")
}

foo_s3.text <- function(x, ...) {
  paste0(x, "-foo")
}

library(methods)
setOldClass(c("number", "numeric", "R7_object"))
setOldClass(c("text", "character", "R7_object"))

setGeneric("foo_s4", function(x, ...) standardGeneric("foo_s4"))
#> [1] "foo_s4"
setMethod("foo_s4", c("text"), function(x, ...) paste0(x, "-foo"))

# Measure performance of single dispatch
bench::mark(foo_R7(x), foo_s3(x), foo_s4(x))
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 foo_R7(x)     8.6µs   9.99µs    83611.        0B     16.7
#> 2 foo_s3(x)    3.87µs   4.31µs   213838.        0B     21.4
#> 3 foo_s4(x)    3.98µs    4.6µs   207822.        0B     20.8

bar_R7 <- new_generic("bar_R7", c("x", "y"))
method(bar_R7, list("text", "number")) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_s4", function(x, y, ...) standardGeneric("bar_s4"))
#> [1] "bar_s4"
setMethod("bar_s4", c("text", "number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_R7(x, y), bar_s4(x, y))
#> # A tibble: 2 x 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_R7(x, y)  14.41µs   17.4µs    57804.        0B     28.9
#> 2 bar_s4(x, y)   9.48µs   10.9µs    87914.        0B     17.6
```

A potential optimization is caching based on the class names, but lookup
should be fast without this.

The following benchmark generates a class heiarchy of different levels
and lengths of class names and compares the time to dispatch on the
first class in the hiearchy vs the time to dispatch on the last class.

We find that even in very extreme cases (e.g. 100 deep heirachy 100 of
character class names) the overhead is reasonable, and for more
reasonable cases (e.g. 10 deep hiearchy of 15 character class names) the
overhead is basically negligible.

``` r
library(R7)

gen_character <- function (n, min = 5, max = 25, values = c(letters, LETTERS, 0:9)) {
  lengths <- sample(min:max, replace = TRUE, size = n)
  values <- sample(values, sum(lengths), replace = TRUE)
  starts <- c(1, cumsum(lengths)[-n] + 1)
  ends <- cumsum(lengths)
  mapply(function(start, end) paste0(values[start:end], collapse=""), starts, ends)
}

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_size = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    text <- new_class("text", parent = "character", constructor = function(text) new_object(.data = text))
    parent <- text
    classes <- gen_character(num_classes, min = class_size, max = class_size)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent, constructor = function(text) new_object(.data = text)), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))

    # Define a generic and a method for the last class (best case scenario)
    foo_R7 <- new_generic(name = "foo_R7", signature = "x")
    method(foo_R7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_R7 <- new_generic(name = "foo2_R7", signature = "x")
    method(foo2_R7, R7_object) <- function(x, ...) paste0(x, "-foo")

    bench::mark(
      best = foo_R7(x),
      worst = foo2_R7(x)
    )
  }
)
#> # A tibble: 20 x 8
#>    expression num_classes class_size      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>      <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3         15   9.91µs   12.4µs    80813.        0B     24.3
#>  2 worst                3         15  10.17µs     12µs    80288.        0B     32.1
#>  3 best                 5         15   9.91µs   12.7µs    76523.        0B     30.6
#>  4 worst                5         15  10.54µs   12.4µs    78195.        0B     31.3
#>  5 best                10         15   9.89µs   12.5µs    78052.        0B     31.2
#>  6 worst               10         15  11.01µs   13.3µs    74023.        0B     29.6
#>  7 best                50         15  10.03µs   11.8µs    81389.        0B     32.6
#>  8 worst               50         15  14.29µs   15.6µs    59115.        0B     23.7
#>  9 best               100         15   10.8µs   12.5µs    77170.        0B     30.9
#> 10 worst              100         15  18.07µs   20.1µs    47337.        0B     18.9
#> 11 best                 3        100   9.82µs   11.3µs    84505.        0B     33.8
#> 12 worst                3        100  10.23µs   12.8µs    76000.        0B     30.4
#> 13 best                 5        100   10.2µs   11.7µs    80082.        0B     32.0
#> 14 worst                5        100  10.99µs   13.8µs    72780.        0B     29.1
#> 15 best                10        100  10.14µs   12.8µs    78083.        0B     31.2
#> 16 worst               10        100  11.85µs   13.2µs    70176.        0B     28.1
#> 17 best                50        100  10.42µs   12.5µs    77965.        0B     31.2
#> 18 worst               50        100  16.82µs   18.5µs    51479.        0B     20.6
#> 19 best               100        100  10.27µs   12.6µs    77110.        0B     30.9
#> 20 worst              100        100   24.2µs   26.1µs    36342.        0B     14.5
```

And the same benchmark using double-dispatch vs single dispatch

``` r
bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_size = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    text <- new_class("text", parent = "character", constructor = function(text) new_object(.data = text))
    parent <- text
    classes <- gen_character(num_classes, min = class_size, max = class_size)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent, constructor = function(text) new_object(.data = text)), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))
    y <- do.call(cls, list("ho"))

    # Define a generic and a method for the last class (best case scenario)
    foo_R7 <- new_generic(name = "foo_R7", signature = c("x", "y"))
    method(foo_R7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_R7 <- new_generic(name = "foo2_R7", signature = c("x", "y"))
    method(foo2_R7, list(R7_object, R7_object)) <- function(x, y, ...) paste0(x, y, "-foo")

    bench::mark(
      best = foo_R7(x, y),
      worst = foo2_R7(x, y)
    )
  }
)
#> # A tibble: 20 x 8
#>    expression num_classes class_size      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>      <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3         15   10.2µs   11.5µs    80091.        0B     32.0
#>  2 worst                3         15     11µs   11.7µs    81925.        0B     32.8
#>  3 best                 5         15   10.3µs   11.5µs    77925.        0B     39.0
#>  4 worst                5         15   11.1µs   11.9µs    81456.        0B     32.6
#>  5 best                10         15   10.7µs   11.7µs    79036.        0B     31.6
#>  6 worst               10         15   12.2µs   13.4µs    70143.        0B     28.1
#>  7 best                50         15     11µs   11.8µs    80724.        0B     40.4
#>  8 worst               50         15   18.3µs   19.5µs    47408.        0B     19.0
#>  9 best               100         15   11.7µs   12.8µs    73903.        0B     29.6
#> 10 worst              100         15   27.1µs   28.8µs    32861.        0B     16.4
#> 11 best                 3        100   10.6µs   11.6µs    81519.        0B     32.6
#> 12 worst                3        100   11.6µs   12.5µs    75169.        0B     30.1
#> 13 best                 5        100   10.7µs   11.9µs    80362.        0B     40.2
#> 14 worst                5        100   12.1µs   13.1µs    70882.        0B     28.4
#> 15 best                10        100   10.6µs     12µs    78126.        0B     31.3
#> 16 worst               10        100   13.9µs     15µs    63313.        0B     31.7
#> 17 best                50        100   11.4µs     13µs    72297.        0B     28.9
#> 18 worst               50        100   23.6µs   25.2µs    37711.        0B     18.9
#> 19 best               100        100   12.1µs   13.4µs    70365.        0B     28.2
#> 20 worst              100        100     37µs   39.2µs    23647.        0B     11.8
```

## Questions

  - What should happen if you call `new_method()` on a S3 generic?
    1.  Should we create a new R7 generic out of the S3 generic?
    2.  Or just register the R7 object using `registerS3method()`?

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
            `new_object()` to initialize the class.
          - [x] - A validator, a function that takes the object and
            returns NULL if the object is valid, otherwise a character
            vector of error messages.
          - [x] - properties, a list of property objects
  - Initialization
      - [x] - The constructor uses `new_object()` to initialize a new
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
      - [x] - Used in properties to allow a property to be one of a set
        of classes
      - [x] - In method dispatch as a convenience for defining a method
        for multiple classes
  - Properties
      - [x] - Accessed using `property()` / `property<-`
      - [x] - Accessed using `@` / `@<-`
      - [x] - A name, used to label output
      - [ ] - A optional class or union
      - [x] - An optional accessor functions, both getter and setters
      - [ ] - Properties are created with `new_prop()`
  - Generics
      - [x] - It knows its name and the names of the arguments in its
        signature
      - [x] - Calling `new_generic()` defines a new generic
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
          - [x] - method is a compatible function
          - [x] - `new_method` is designed to work at run-time
              - [x] - `new_method` should optionally take a package
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
          - [x] - `next_method()` can dispatch on multiple arguments,
            avoiding methods that have already been called.
          - [x] - Generics forward promises to methods, so methods can
            use non-standard evaluation.
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
