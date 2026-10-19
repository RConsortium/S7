# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text := new_class(parent = class_character)
Number := new_class(parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 := new_generic("x")
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
#> 1 foo_S7(x)    3.51µs   4.01µs   238537.    10.9KB     47.7
#> 2 foo_S3(x)    1.52µs   1.71µs   534460.        0B     53.5
#> 3 foo_S4(x)    1.57µs   1.79µs   538483.        0B     53.9

bar_S7 := new_generic(c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)   7.23µs   8.24µs   115759.        0B     46.3
#> 2 bar_S4(x, y)   4.07µs   4.54µs   215991.        0B     43.2
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
    Text := new_class(parent = class_character)
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
    foo_S7 := new_generic("x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic("x")
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
#>  1 best                 3          15   3.58µs   4.14µs   231733.        0B     46.4
#>  2 worst                3          15   3.71µs   4.14µs   236713.        0B     47.4
#>  3 best                 5          15   3.62µs   3.99µs   244866.        0B     49.0
#>  4 worst                5          15   3.75µs   4.08µs   240382.        0B     48.1
#>  5 best                10          15   3.58µs   3.97µs   246613.        0B     49.3
#>  6 worst               10          15   3.79µs    4.1µs   238799.        0B     47.8
#>  7 best                50          15   3.71µs   4.06µs   241170.        0B     48.2
#>  8 worst               50          15   4.82µs   5.12µs   192086.        0B     38.4
#>  9 best               100          15   3.85µs   4.29µs   228068.        0B     45.6
#> 10 worst              100          15   6.08µs   6.57µs   149897.        0B     30.0
#> 11 best                 3         100   3.67µs   4.12µs   212743.        0B     42.6
#> 12 worst                3         100   3.91µs   4.26µs   229880.        0B     46.0
#> 13 best                 5         100   3.62µs   4.04µs   240872.        0B     48.2
#> 14 worst                5         100   3.97µs    4.3µs   228188.        0B     45.6
#> 15 best                10         100   3.67µs   4.07µs   239399.        0B     47.9
#> 16 worst               10         100   4.15µs   4.51µs   217267.        0B     43.5
#> 17 best                50         100   3.79µs   4.17µs   233909.        0B     46.8
#> 18 worst               50         100    7.1µs   7.44µs   132055.        0B     26.4
#> 19 best               100         100   3.97µs    4.4µs   222552.        0B     44.5
#> 20 worst              100         100  10.99µs  11.37µs    86839.        0B     17.4
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text := new_class(parent = class_character)
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
    foo_S7 := new_generic(c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic(c("x", "y"))
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
#>  1 best                 3          15   5.15µs   5.88µs   163358.        0B     49.0
#>  2 worst                3          15   5.37µs   5.93µs   164685.        0B     49.4
#>  3 best                 5          15    5.3µs    5.9µs   165677.        0B     49.7
#>  4 worst                5          15   5.43µs   5.94µs   164905.        0B     49.5
#>  5 best                10          15   5.18µs   5.67µs   172190.        0B     51.7
#>  6 worst               10          15   5.66µs   6.15µs   158595.        0B     47.6
#>  7 best                50          15   5.38µs   5.96µs   159449.        0B     47.8
#>  8 worst               50          15   7.55µs   8.13µs   120792.        0B     36.2
#>  9 best               100          15   5.75µs   6.37µs   153671.        0B     46.1
#> 10 worst              100          15    9.7µs   10.6µs    92991.        0B     18.6
#> 11 best                 3         100   5.15µs   5.74µs   170748.        0B     34.2
#> 12 worst                3         100   5.81µs   6.31µs   154875.        0B     46.5
#> 13 best                 5         100   5.15µs   5.65µs   173402.        0B     34.7
#> 14 worst                5         100   5.88µs   6.43µs   152055.        0B     45.6
#> 15 best                10         100   5.07µs   5.64µs   173295.        0B     52.0
#> 16 worst               10         100   6.54µs   6.99µs   140395.        0B     42.1
#> 17 best                50         100   5.54µs   6.05µs   161032.        0B     32.2
#> 18 worst               50         100  11.35µs  11.85µs    83176.        0B     25.0
#> 19 best               100         100   5.74µs   6.25µs   156753.        0B     31.4
#> 20 worst              100         100  17.61µs  18.54µs    53448.        0B     16.0
```
