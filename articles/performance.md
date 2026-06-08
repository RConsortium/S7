# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text <- new_class("Text", parent = class_character)
Number <- new_class("Number", parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 <- new_generic("foo_S7", "x")
method(foo_S7, Text) <- function(x, ...) paste0(x, "-foo")

foo_S3 <- function(x, ...) {
  UseMethod("foo_S3")
}

foo_S3.Text <- function(x, ...) {
  paste0(x, "-foo")
}

library(methods)
setOldClass(c("Number", "numeric", "S7_object"))
setOldClass(c("Text", "character", "S7_object"))

setGeneric("foo_S4", function(x, ...) standardGeneric("foo_S4"))
#> [1] "foo_S4"
setMethod("foo_S4", c("Text"), function(x, ...) paste0(x, "-foo"))

# Measure performance of single dispatch
bench::mark(foo_S7(x), foo_S3(x), foo_S4(x))
#> # A tibble: 3 × 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 foo_S7(x)    7.33µs   8.86µs   105877.    10.8KB     31.8
#> 2 foo_S3(x)     2.5µs   2.87µs   315704.        0B     31.6
#> 3 foo_S4(x)    2.68µs   3.23µs   297797.        0B     29.8

bar_S7 <- new_generic("bar_S7", c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)  13.06µs  15.14µs    64107.        0B     25.7
#> 2 bar_S4(x, y)   6.92µs   8.04µs   120925.        0B     24.2
```

A potential optimization is caching based on the class names, but lookup
should be fast without this.

The following benchmark generates a class hierarchy of different levels
and lengths of class names and compares the time to dispatch on the
first class in the hierarchy vs the time to dispatch on the last class.

We find that even in very extreme cases (e.g. 100 deep hierarchy 100 of
character class names) the overhead is reasonable, and for more
reasonable cases (e.g. 10 deep hierarchy of 15 character class names)
the overhead is basically negligible.

``` r

library(S7)

gen_character <- function (n, min = 5, max = 25, values = c(letters, LETTERS, 0:9)) {
  lengths <- sample(min:max, replace = TRUE, size = n)
  values <- sample(values, sum(lengths), replace = TRUE)
  starts <- c(1, cumsum(lengths)[-n] + 1)
  ends <- cumsum(lengths)
  mapply(function(start, end) paste0(values[start:end], collapse=""), starts, ends)
}

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text <- new_class("Text", parent = class_character)
    parent <- Text
    classes <- gen_character(num_classes, min = class_nchar, max = class_nchar)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))

    # Define a generic and a method for the last class (best case scenario)
    foo_S7 <- new_generic("foo_S7", "x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 <- new_generic("foo2_S7", "x")
    method(foo2_S7, S7_object) <- function(x, ...) paste0(x, "-foo")

    bench::mark(
      best = foo_S7(x),
      worst = foo2_S7(x)
    )
  }
)
#> # A tibble: 20 × 8
#>    expression num_classes class_nchar      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>       <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3          15   7.48µs   9.08µs   106237.        0B     31.9
#>  2 worst                3          15   7.64µs    9.3µs   103922.        0B     31.2
#>  3 best                 5          15   7.43µs   9.17µs   104605.        0B     31.4
#>  4 worst                5          15   7.78µs   9.43µs   102424.        0B     30.7
#>  5 best                10          15    7.7µs   9.39µs   102869.        0B     30.9
#>  6 worst               10          15   7.99µs   9.71µs    99610.        0B     29.9
#>  7 best                50          15   7.96µs   9.73µs    99301.        0B     29.8
#>  8 worst               50          15   9.67µs  11.41µs    84650.        0B     25.4
#>  9 best               100          15   8.72µs  10.59µs    90657.        0B     27.2
#> 10 worst              100          15  11.89µs  13.74µs    69905.        0B     28.0
#> 11 best                 3         100   7.62µs   9.41µs   102603.        0B     30.8
#> 12 worst                3         100   7.76µs   9.64µs   100198.        0B     30.1
#> 13 best                 5         100   7.54µs   9.35µs   101144.        0B     30.4
#> 14 worst                5         100   7.88µs   9.74µs    97163.        0B     29.2
#> 15 best                10         100   7.54µs   9.31µs   101444.        0B     30.4
#> 16 worst               10         100   8.26µs  10.18µs    90533.        0B     27.2
#> 17 best                50         100    8.1µs   9.95µs    94958.        0B     28.5
#> 18 worst               50         100  13.18µs   15.1µs    63611.        0B     19.1
#> 19 best               100         100   8.76µs  10.74µs    88500.        0B     26.6
#> 20 worst              100         100  19.36µs  21.27µs    45495.        0B     13.7
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text <- new_class("Text", parent = class_character)
    parent <- Text
    classes <- gen_character(num_classes, min = class_nchar, max = class_nchar)
    env <- new.env()
    for (x in classes) {
      assign(x, new_class(x, parent = parent), env)
      parent <- get(x, env)
    }

    # Get the last defined class
    cls <- parent

    # Construct an object of that class
    x <- do.call(cls, list("hi"))
    y <- do.call(cls, list("ho"))

    # Define a generic and a method for the last class (best case scenario)
    foo_S7 <- new_generic("foo_S7", c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 <- new_generic("foo2_S7", c("x", "y"))
    method(foo2_S7, list(S7_object, S7_object)) <- function(x, y, ...) paste0(x, y, "-foo")

    bench::mark(
      best = foo_S7(x, y),
      worst = foo2_S7(x, y)
    )
  }
)
#> # A tibble: 20 × 8
#>    expression num_classes class_nchar      min   median `itr/sec` mem_alloc `gc/sec`
#>    <bch:expr>       <dbl>       <dbl> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#>  1 best                 3          15   9.23µs   11.3µs    83399.        0B     25.0
#>  2 worst                3          15   9.62µs   11.8µs    79834.        0B     31.9
#>  3 best                 5          15   9.43µs   11.5µs    82000.        0B     32.8
#>  4 worst                5          15   9.54µs   11.1µs    85824.        0B     34.3
#>  5 best                10          15   9.39µs   10.2µs    94077.        0B     37.6
#>  6 worst               10          15  10.14µs     11µs    88646.        0B     35.5
#>  7 best                50          15  10.56µs   11.4µs    85616.        0B     34.3
#>  8 worst               50          15  13.71µs   14.6µs    66222.        0B     26.5
#>  9 best               100          15  11.78µs   13.1µs    72636.        0B     29.1
#> 10 worst              100          15  18.35µs   19.8µs    48761.        0B     19.5
#> 11 best                 3         100   9.47µs   10.9µs    87150.        0B     34.9
#> 12 worst                3         100  10.38µs   11.8µs    80790.        0B     32.3
#> 13 best                 5         100   9.42µs     11µs    86116.        0B     34.5
#> 14 worst                5         100   10.5µs   12.1µs    78175.        0B     31.3
#> 15 best                10         100   9.68µs   11.2µs    83915.        0B     33.6
#> 16 worst               10         100  11.89µs   13.5µs    70195.        0B     28.1
#> 17 best                50         100  10.76µs   12.4µs    75956.        0B     30.4
#> 18 worst               50         100  19.84µs   21.6µs    44512.        0B     17.8
#> 19 best               100         100  11.93µs   13.4µs    70221.        0B     35.1
#> 20 worst              100         100  31.18µs   32.7µs    29623.        0B     11.9
```
