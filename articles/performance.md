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
#> 1 foo_S7(x)    3.88µs   4.92µs   187655.    10.8KB     37.5
#> 2 foo_S3(x)    1.47µs   1.65µs   544152.        0B      0  
#> 3 foo_S4(x)    1.55µs   1.87µs   513096.        0B     51.3

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
#> 1 bar_S7(x, y)   7.13µs   8.56µs   112450.        0B     33.7
#> 2 bar_S4(x, y)   3.89µs   4.55µs   211985.        0B     42.4
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
#>  1 best                 3          15   3.97µs   5.04µs   189640.        0B     37.9
#>  2 worst                3          15   4.05µs   5.21µs   181922.        0B     54.6
#>  3 best                 5          15   3.97µs   5.12µs   184763.        0B     55.4
#>  4 worst                5          15    4.1µs   5.23µs   180832.        0B     54.3
#>  5 best                10          15   4.03µs   5.16µs   183711.        0B     55.1
#>  6 worst               10          15   4.19µs   5.27µs   182267.        0B     36.5
#>  7 best                50          15   4.31µs   5.47µs   173969.        0B     52.2
#>  8 worst               50          15   5.32µs   6.49µs   146792.        0B     44.1
#>  9 best               100          15   4.69µs   5.81µs   164024.        0B     49.2
#> 10 worst              100          15   6.74µs   7.88µs   121934.        0B     36.6
#> 11 best                 3         100   4.05µs    5.2µs   182617.        0B     54.8
#> 12 worst                3         100   4.23µs   5.39µs   175773.        0B     52.7
#> 13 best                 5         100   4.04µs   5.21µs   180754.        0B     72.3
#> 14 worst                5         100   4.25µs   5.44µs   172727.        0B     34.6
#> 15 best                10         100   4.05µs   5.27µs   175608.        0B     35.1
#> 16 worst               10         100   4.46µs   5.75µs   158655.        0B     47.6
#> 17 best                50         100   4.32µs   5.57µs   161702.        0B     48.5
#> 18 worst               50         100   7.47µs   8.76µs   108255.        0B     32.5
#> 19 best               100         100   4.84µs   6.15µs   149642.        0B     44.9
#> 20 worst              100         100  11.49µs  12.85µs    74211.        0B     22.3
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
#>  1 best                 3          15   5.11µs   6.68µs   137768.        0B     41.3
#>  2 worst                3          15   5.28µs   6.99µs   129810.        0B     51.9
#>  3 best                 5          15   5.12µs    6.7µs   135531.        0B     54.2
#>  4 worst                5          15   5.41µs   7.01µs   131365.        0B     39.4
#>  5 best                10          15   5.19µs   6.76µs   133991.        0B     53.6
#>  6 worst               10          15   5.56µs   6.64µs   141599.        0B     42.5
#>  7 best                50          15   5.79µs   6.65µs   146585.        0B     44.0
#>  8 worst               50          15   7.59µs   8.53µs   114666.        0B     45.9
#>  9 best               100          15   6.51µs   7.45µs   131072.        0B     65.6
#> 10 worst              100          15  10.36µs  11.56µs    82532.        0B     33.0
#> 11 best                 3         100   5.25µs   6.53µs   142291.        0B     42.7
#> 12 worst                3         100   5.69µs   7.15µs   129593.        0B     51.9
#> 13 best                 5         100   5.14µs   6.46µs   144203.        0B     43.3
#> 14 worst                5         100   5.88µs   7.16µs   129742.        0B     51.9
#> 15 best                10         100   5.16µs   6.46µs   144795.        0B     43.5
#> 16 worst               10         100   6.61µs   7.93µs   117150.        0B     46.9
#> 17 best                50         100   5.91µs    7.3µs   126089.        0B     50.5
#> 18 worst               50         100  11.65µs  12.99µs    73675.        0B     22.1
#> 19 best               100         100   6.72µs   7.95µs   116150.        0B     58.1
#> 20 worst              100         100  18.65µs  20.14µs    47663.        0B     14.3
```
