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
#> 1 foo_S7(x)    7.17µs   8.11µs   114905.    18.2KB     23.0
#> 2 foo_S3(x)    2.57µs    2.8µs   326224.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.07µs   309289.        0B     30.9

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
#> 1 bar_S7(x, y)  13.15µs   14.4µs    66673.        0B     26.7
#> 2 bar_S4(x, y)   7.36µs    8.1µs   120762.        0B     24.2
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
#>  1 best                 3          15   7.25µs   8.41µs   115657.        0B     34.7
#>  2 worst                3          15   7.44µs    8.7µs   110948.        0B     22.2
#>  3 best                 5          15   7.27µs   8.37µs   116422.        0B     23.3
#>  4 worst                5          15   7.42µs   8.47µs   114369.        0B     34.3
#>  5 best                10          15   7.32µs   8.38µs   115521.        0B     34.7
#>  6 worst               10          15   7.63µs    8.9µs   109243.        0B     21.9
#>  7 best                50          15   7.75µs   8.83µs   109921.        0B     33.0
#>  8 worst               50          15    9.4µs  10.41µs    93247.        0B     18.7
#>  9 best               100          15   8.46µs   9.44µs   102647.        0B     30.8
#> 10 worst              100          15  11.69µs  12.74µs    76420.        0B     22.9
#> 11 best                 3         100   7.24µs   8.35µs   115967.        0B     34.8
#> 12 worst                3         100   7.54µs   8.62µs   112536.        0B     22.5
#> 13 best                 5         100   7.25µs   8.43µs   114065.        0B     22.8
#> 14 worst                5         100   7.57µs   8.64µs   112263.        0B     33.7
#> 15 best                10         100   7.32µs   8.34µs   115937.        0B     34.8
#> 16 worst               10         100      8µs    9.1µs   105072.        0B     31.5
#> 17 best                50         100   7.87µs   9.02µs   106983.        0B     21.4
#> 18 worst               50         100  12.64µs  13.92µs    69973.        0B     21.0
#> 19 best               100         100   8.58µs    9.8µs    98084.        0B     29.4
#> 20 worst              100         100  19.17µs   20.4µs    47356.        0B     14.2
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
#>  1 best                 3          15    8.9µs   10.3µs    93092.        0B     37.3
#>  2 worst                3          15   9.23µs   10.4µs    92072.        0B     27.6
#>  3 best                 5          15   9.08µs   10.2µs    94256.        0B     37.7
#>  4 worst                5          15    9.5µs   10.9µs    86436.        0B     25.9
#>  5 best                10          15   9.11µs   10.4µs    91773.        0B     27.5
#>  6 worst               10          15   9.85µs   11.1µs    86818.        0B     34.7
#>  7 best                50          15   9.97µs   10.7µs    90499.        0B     27.2
#>  8 worst               50          15   13.1µs   13.7µs    71169.        0B     28.5
#>  9 best               100          15  11.35µs     12µs    81142.        0B     32.5
#> 10 worst              100          15  17.49µs   18.3µs    53333.        0B     21.3
#> 11 best                 3         100    9.2µs     10µs    96098.        0B     38.5
#> 12 worst                3         100   9.86µs   11.1µs    87579.        0B     26.3
#> 13 best                 5         100   9.13µs   10.2µs    95348.        0B     38.2
#> 14 worst                5         100  10.04µs   11.1µs    87053.        0B     26.1
#> 15 best                10         100   9.25µs   10.4µs    93493.        0B     28.1
#> 16 worst               10         100   11.4µs   12.6µs    77398.        0B     31.0
#> 17 best                50         100  10.52µs   11.6µs    83858.        0B     25.2
#> 18 worst               50         100  19.39µs   20.5µs    47570.        0B     19.0
#> 19 best               100         100  11.66µs   12.6µs    77028.        0B     30.8
#> 20 worst              100         100  30.05µs   31.3µs    31225.        0B     15.6
```
