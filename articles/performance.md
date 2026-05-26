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
#> 1 foo_S7(x)    7.23µs   8.74µs   107399.    10.8KB     21.5
#> 2 foo_S3(x)     2.5µs   2.83µs   321154.        0B      0  
#> 3 foo_S4(x)    2.69µs   3.21µs   299932.        0B     30.0

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
#> 1 bar_S7(x, y)  12.57µs  14.88µs    65276.        0B     19.6
#> 2 bar_S4(x, y)   6.89µs   7.93µs   122619.        0B     24.5
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
#>  1 best                 3          15   7.28µs    8.9µs   109269.        0B     21.9
#>  2 worst                3          15    7.4µs   8.99µs   108103.        0B     32.4
#>  3 best                 5          15   7.34µs   8.92µs   108553.        0B     32.6
#>  4 worst                5          15   7.68µs   9.26µs   104819.        0B     31.5
#>  5 best                10          15   7.52µs   9.09µs   106712.        0B     32.0
#>  6 worst               10          15   7.79µs   9.31µs   104279.        0B     20.9
#>  7 best                50          15   7.81µs   9.42µs   102755.        0B     30.8
#>  8 worst               50          15   9.59µs   11.1µs    87630.        0B     26.3
#>  9 best               100          15   8.74µs  10.28µs    94588.        0B     28.4
#> 10 worst              100          15  12.11µs  13.69µs    71245.        0B     28.5
#> 11 best                 3         100    7.6µs   9.17µs   105746.        0B     31.7
#> 12 worst                3         100   7.91µs   9.46µs   102429.        0B     30.7
#> 13 best                 5         100   7.64µs   9.27µs   104804.        0B     31.5
#> 14 worst                5         100   7.97µs   9.67µs    99895.        0B     30.0
#> 15 best                10         100   7.47µs   9.12µs   105143.        0B     31.6
#> 16 worst               10         100   8.27µs   9.91µs    95266.        0B     28.6
#> 17 best                50         100   8.08µs   9.74µs    98631.        0B     29.6
#> 18 worst               50         100  13.12µs  14.92µs    64283.        0B     19.3
#> 19 best               100         100    8.8µs  10.51µs    91680.        0B     18.3
#> 20 worst              100         100  19.42µs  21.31µs    45692.        0B     13.7
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
#>  1 best                 3          15    9.3µs   11.1µs    86595.        0B     34.7
#>  2 worst                3          15   9.42µs   11.4µs    84672.        0B     25.4
#>  3 best                 5          15   9.28µs   11.2µs    85959.        0B     34.4
#>  4 worst                5          15   9.72µs   11.5µs    83739.        0B     25.1
#>  5 best                10          15   9.36µs   11.2µs    85285.        0B     34.1
#>  6 worst               10          15  10.16µs   11.2µs    85444.        0B     25.6
#>  7 best                50          15  10.45µs   11.2µs    86997.        0B     26.1
#>  8 worst               50          15  13.73µs   14.5µs    67550.        0B     27.0
#>  9 best               100          15  11.73µs   12.5µs    77917.        0B     31.2
#> 10 worst              100          15  18.48µs   19.7µs    49246.        0B     19.7
#> 11 best                 3         100   9.56µs     11µs    87881.        0B     35.2
#> 12 worst                3         100  10.41µs   11.8µs    81308.        0B     24.4
#> 13 best                 5         100   9.45µs     11µs    87376.        0B     35.0
#> 14 worst                5         100  10.47µs   11.8µs    81209.        0B     24.4
#> 15 best                10         100   9.54µs     11µs    86675.        0B     34.7
#> 16 worst               10         100  11.69µs   13.1µs    74043.        0B     22.2
#> 17 best                50         100  10.71µs   12.1µs    79002.        0B     23.7
#> 18 worst               50         100  20.08µs   21.7µs    44935.        0B     18.0
#> 19 best               100         100  11.89µs   13.3µs    72377.        0B     29.0
#> 20 worst              100         100  31.36µs     33µs    29536.        0B     11.8
```
