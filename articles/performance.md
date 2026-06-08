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
#> 1 foo_S7(x)    6.81µs   9.38µs   100354.    10.8KB     30.1
#> 2 foo_S3(x)    2.32µs   2.88µs   299696.        0B     30.0
#> 3 foo_S4(x)     2.5µs   3.08µs   294394.        0B     29.4

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
#> 1 bar_S7(x, y)  12.14µs  15.76µs    61133.        0B     24.5
#> 2 bar_S4(x, y)   6.54µs   8.06µs   118307.        0B     23.7
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
#>  1 best                 3          15   6.88µs   9.57µs   100470.        0B     30.2
#>  2 worst                3          15   6.95µs   9.81µs    98429.        0B     29.5
#>  3 best                 5          15   6.89µs   9.58µs   100338.        0B     30.1
#>  4 worst                5          15    7.1µs   9.81µs    98194.        0B     29.5
#>  5 best                10          15   6.85µs   9.61µs    99976.        0B     30.0
#>  6 worst               10          15   7.18µs  10.01µs    96267.        0B     28.9
#>  7 best                50          15   7.25µs  10.06µs    95342.        0B     28.6
#>  8 worst               50          15   8.68µs  11.57µs    83178.        0B     25.0
#>  9 best               100          15   7.69µs  10.57µs    90806.        0B     27.3
#> 10 worst              100          15   10.6µs  13.65µs    70695.        0B     21.2
#> 11 best                 3         100   6.85µs   9.81µs    97726.        0B     29.3
#> 12 worst                3         100   7.25µs  10.11µs    95045.        0B     28.5
#> 13 best                 5         100   6.96µs   9.85µs    96211.        0B     28.9
#> 14 worst                5         100    7.3µs  10.15µs    93569.        0B     28.1
#> 15 best                10         100   6.93µs   9.81µs    96669.        0B     29.0
#> 16 worst               10         100   7.57µs  10.45µs    91039.        0B     27.3
#> 17 best                50         100   7.28µs  10.11µs    93706.        0B     28.1
#> 18 worst               50         100  11.59µs  14.65µs    65655.        0B     19.7
#> 19 best               100         100   7.74µs  10.63µs    89009.        0B     26.7
#> 20 worst              100         100  16.91µs  20.07µs    48111.        0B     14.4
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
#>  1 best                 3          15   8.51µs     12µs    78936.        0B     31.6
#>  2 worst                3          15   8.84µs   12.3µs    76481.        0B     30.6
#>  3 best                 5          15   8.56µs   11.9µs    79337.        0B     31.7
#>  4 worst                5          15   8.87µs   11.4µs    82682.        0B     33.1
#>  5 best                10          15   8.48µs    9.4µs   101908.        0B     40.8
#>  6 worst               10          15   9.27µs   10.2µs    94560.        0B     28.4
#>  7 best                50          15   9.19µs   10.1µs    94334.        0B     37.7
#>  8 worst               50          15  12.08µs     13µs    73665.        0B     22.1
#>  9 best               100          15  10.16µs   11.6µs    80760.        0B     32.3
#> 10 worst              100          15  15.85µs   17.4µs    54780.        0B     21.9
#> 11 best                 3         100   8.61µs     10µs    92939.        0B     37.2
#> 12 worst                3         100   9.34µs   10.8µs    86034.        0B     43.0
#> 13 best                 5         100   8.47µs   10.1µs    90922.        0B     36.4
#> 14 worst                5         100   9.38µs   11.1µs    83974.        0B     33.6
#> 15 best                10         100   8.56µs   10.2µs    90572.        0B     36.2
#> 16 worst               10         100  10.51µs   12.2µs    76076.        0B     22.8
#> 17 best                50         100   9.46µs   11.2µs    83146.        0B     33.3
#> 18 worst               50         100  17.61µs   19.5µs    48767.        0B     14.6
#> 19 best               100         100  10.33µs   12.3µs    75392.        0B     37.7
#> 20 worst              100         100  27.07µs   29.2µs    33228.        0B     16.6
```
