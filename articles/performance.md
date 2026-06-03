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
#> 1 foo_S7(x)     6.7µs   8.15µs   115356.    10.8KB     23.1
#> 2 foo_S3(x)     2.3µs    2.7µs   333500.        0B      0  
#> 3 foo_S4(x)    2.52µs   2.87µs   329282.        0B     32.9

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
#> 1 bar_S7(x, y)   11.8µs  13.97µs    69619.        0B     20.9
#> 2 bar_S4(x, y)    6.5µs   7.38µs   131254.        0B     26.3
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
#>  1 best                 3          15   6.81µs   8.32µs   116535.        0B     23.3
#>  2 worst                3          15   7.01µs   8.52µs   113070.        0B     33.9
#>  3 best                 5          15   6.82µs   8.37µs   115197.        0B     34.6
#>  4 worst                5          15   7.02µs   8.55µs   112906.        0B     33.9
#>  5 best                10          15   6.75µs   8.35µs   115382.        0B     34.6
#>  6 worst               10          15   7.16µs   8.72µs   111055.        0B     22.2
#>  7 best                50          15   7.19µs   8.81µs   107951.        0B     32.4
#>  8 worst               50          15   8.54µs  10.22µs    94741.        0B     28.4
#>  9 best               100          15    7.7µs   9.32µs   103585.        0B     31.1
#> 10 worst              100          15  10.53µs  12.31µs    78934.        0B     23.7
#> 11 best                 3         100   6.85µs   8.43µs   114242.        0B     34.3
#> 12 worst                3         100   7.21µs   8.69µs   110870.        0B     33.3
#> 13 best                 5         100    6.9µs    8.4µs   114604.        0B     34.4
#> 14 worst                5         100   7.25µs    8.8µs   109170.        0B     21.8
#> 15 best                10         100   6.91µs   8.54µs   112516.        0B     22.5
#> 16 worst               10         100   7.46µs   9.25µs   101528.        0B     30.5
#> 17 best                50         100   7.14µs    8.9µs   107665.        0B     32.3
#> 18 worst               50         100  11.62µs  13.35µs    72646.        0B     14.5
#> 19 best               100         100   7.73µs   9.51µs   101219.        0B     30.4
#> 20 worst              100         100  16.95µs  18.79µs    52003.        0B     15.6
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
#>  1 best                 3          15   8.42µs  10.32µs    92972.        0B     27.9
#>  2 worst                3          15   8.75µs  10.72µs    89112.        0B     35.7
#>  3 best                 5          15   8.52µs  10.39µs    92094.        0B     36.9
#>  4 worst                5          15   8.91µs  10.88µs    88244.        0B     26.5
#>  5 best                10          15   8.58µs  10.55µs    90646.        0B     36.3
#>  6 worst               10          15    9.1µs  10.35µs    91728.        0B     27.5
#>  7 best                50          15   9.18µs   9.76µs    99071.        0B     39.6
#>  8 worst               50          15  12.04µs  12.66µs    76797.        0B     30.7
#>  9 best               100          15  10.09µs  10.71µs    90559.        0B     45.3
#> 10 worst              100          15  15.81µs  16.93µs    57195.        0B     22.9
#> 11 best                 3         100   8.48µs   9.62µs    99366.        0B     29.8
#> 12 worst                3         100   9.28µs  10.55µs    90869.        0B     36.4
#> 13 best                 5         100   8.31µs    9.5µs   100240.        0B     30.1
#> 14 worst                5         100   9.33µs  10.65µs    89858.        0B     36.0
#> 15 best                10         100   8.48µs    9.6µs    99812.        0B     30.0
#> 16 worst               10         100  10.42µs  11.72µs    81996.        0B     32.8
#> 17 best                50         100    9.4µs  10.72µs    89096.        0B     35.7
#> 18 worst               50         100  17.39µs  18.72µs    51797.        0B     15.5
#> 19 best               100         100  10.24µs  11.32µs    84691.        0B     33.9
#> 20 worst              100         100  26.87µs  28.21µs    34680.        0B     10.4
```
