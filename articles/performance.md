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
#> 1 foo_S7(x)    7.13µs   8.23µs   113012.    18.2KB     22.6
#> 2 foo_S3(x)    2.55µs   2.79µs   327844.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.06µs   313930.        0B     31.4

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
#> 1 bar_S7(x, y)  13.13µs   14.6µs    66303.        0B     26.5
#> 2 bar_S4(x, y)   7.29µs   8.11µs   120268.        0B     24.1
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
#>  1 best                 3          15   7.07µs   8.26µs   117596.        0B     35.3
#>  2 worst                3          15   7.39µs   8.37µs   115938.        0B     34.8
#>  3 best                 5          15    7.1µs   8.23µs   117600.        0B     23.5
#>  4 worst                5          15   7.29µs   8.48µs   114510.        0B     34.4
#>  5 best                10          15   7.21µs   8.47µs   114422.        0B     34.3
#>  6 worst               10          15   7.63µs   8.91µs   108783.        0B     32.6
#>  7 best                50          15   7.78µs   9.05µs   107053.        0B     32.1
#>  8 worst               50          15   9.68µs  10.85µs    89556.        0B     26.9
#>  9 best               100          15   8.31µs   9.47µs   102657.        0B     30.8
#> 10 worst              100          15  11.86µs  13.14µs    73971.        0B     22.2
#> 11 best                 3         100   7.37µs   8.52µs   114039.        0B     34.2
#> 12 worst                3         100    7.8µs   9.04µs   107243.        0B     32.2
#> 13 best                 5         100   7.38µs   8.59µs   112718.        0B     33.8
#> 14 worst                5         100   7.67µs   8.91µs   108862.        0B     32.7
#> 15 best                10         100   7.34µs   8.64µs   111010.        0B     33.3
#> 16 worst               10         100   8.18µs   9.51µs   101174.        0B     30.4
#> 17 best                50         100    7.8µs    9.2µs   104375.        0B     20.9
#> 18 worst               50         100  13.16µs  14.45µs    66846.        0B     20.1
#> 19 best               100         100   8.35µs   9.84µs    97530.        0B     29.3
#> 20 worst              100         100   19.7µs  21.16µs    45906.        0B     13.8
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
#>  1 best                 3          15   8.92µs   10.2µs    94308.        0B     28.3
#>  2 worst                3          15    9.3µs   10.4µs    92286.        0B     36.9
#>  3 best                 5          15   8.95µs   10.2µs    94886.        0B     28.5
#>  4 worst                5          15   9.42µs   10.5µs    91883.        0B     36.8
#>  5 best                10          15   9.21µs   10.4µs    93166.        0B     28.0
#>  6 worst               10          15   9.97µs   11.2µs    86216.        0B     25.9
#>  7 best                50          15   9.89µs   10.5µs    93222.        0B     28.0
#>  8 worst               50          15  13.29µs     14µs    68588.        0B     20.6
#>  9 best               100          15  11.14µs   11.8µs    82613.        0B     33.1
#> 10 worst              100          15   18.3µs   19.1µs    51254.        0B     20.5
#> 11 best                 3         100    9.1µs     10µs    96351.        0B     38.6
#> 12 worst                3         100   9.96µs   11.1µs    88047.        0B     26.4
#> 13 best                 5         100      9µs     10µs    96851.        0B     38.8
#> 14 worst                5         100   9.96µs     11µs    87992.        0B     26.4
#> 15 best                10         100   9.09µs   10.1µs    96255.        0B     28.9
#> 16 worst               10         100  11.39µs   12.4µs    78554.        0B     31.4
#> 17 best                50         100  10.42µs   11.5µs    83366.        0B     25.0
#> 18 worst               50         100  19.91µs   21.2µs    46040.        0B     18.4
#> 19 best               100         100  11.45µs   12.5µs    77124.        0B     30.9
#> 20 worst              100         100  31.23µs   32.5µs    30037.        0B     15.0
```
