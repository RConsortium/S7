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
#> 1 foo_S7(x)    7.38µs   8.44µs   110605.    18.2KB     22.1
#> 2 foo_S3(x)    2.56µs   2.79µs   327000.        0B      0  
#> 3 foo_S4(x)    2.79µs   3.13µs   307866.        0B     30.8

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
#> 1 bar_S7(x, y)  13.06µs  14.43µs    67084.        0B     26.8
#> 2 bar_S4(x, y)   7.28µs   7.99µs   122006.        0B     24.4
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
#>  1 best                 3          15   7.32µs   8.51µs   113336.        0B     22.7
#>  2 worst                3          15   7.49µs   8.67µs   111910.        0B     33.6
#>  3 best                 5          15   7.31µs   8.38µs   115748.        0B     34.7
#>  4 worst                5          15   7.61µs   8.78µs   109858.        0B     33.0
#>  5 best                10          15   7.36µs   8.54µs   113665.        0B     22.7
#>  6 worst               10          15   7.69µs   8.79µs   110376.        0B     33.1
#>  7 best                50          15   7.92µs   8.93µs   108890.        0B     32.7
#>  8 worst               50          15   9.62µs  10.57µs    91786.        0B     27.5
#>  9 best               100          15   8.53µs    9.7µs   100180.        0B     30.1
#> 10 worst              100          15  12.19µs  13.31µs    72947.        0B     21.9
#> 11 best                 3         100   7.46µs   8.52µs   113252.        0B     34.0
#> 12 worst                3         100   7.74µs   8.91µs   108487.        0B     32.6
#> 13 best                 5         100   7.44µs    8.5µs   114297.        0B     34.3
#> 14 worst                5         100   7.77µs   8.81µs   109863.        0B     33.0
#> 15 best                10         100    7.4µs   8.53µs   112720.        0B     33.8
#> 16 worst               10         100   8.21µs   9.28µs   104052.        0B     31.2
#> 17 best                50         100   7.82µs   8.81µs   109326.        0B     32.8
#> 18 worst               50         100  13.11µs   14.3µs    67609.        0B     20.3
#> 19 best               100         100    8.5µs   9.62µs   100725.        0B     30.2
#> 20 worst              100         100  19.65µs  20.75µs    46904.        0B     14.1
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
#>  1 best                 3          15   9.02µs   10.2µs    93745.        0B     28.1
#>  2 worst                3          15   9.41µs   10.5µs    91647.        0B     36.7
#>  3 best                 5          15   9.23µs   10.5µs    91627.        0B     27.5
#>  4 worst                5          15   9.28µs   10.7µs    89859.        0B     36.0
#>  5 best                10          15   9.23µs   10.3µs    93424.        0B     37.4
#>  6 worst               10          15    9.8µs   11.1µs    87414.        0B     26.2
#>  7 best                50          15  10.02µs   10.7µs    91036.        0B     36.4
#>  8 worst               50          15  13.41µs     14µs    69737.        0B     20.9
#>  9 best               100          15  11.07µs   11.7µs    83100.        0B     33.3
#> 10 worst              100          15  18.23µs   18.9µs    51569.        0B     20.6
#> 11 best                 3         100   9.05µs   10.2µs    94433.        0B     28.3
#> 12 worst                3         100   10.1µs   11.1µs    87118.        0B     26.1
#> 13 best                 5         100   9.06µs   10.2µs    95218.        0B     28.6
#> 14 worst                5         100  10.23µs   11.4µs    84843.        0B     25.5
#> 15 best                10         100   9.29µs   10.3µs    93687.        0B     28.1
#> 16 worst               10         100   11.5µs   12.6µs    77383.        0B     23.2
#> 17 best                50         100  10.34µs   11.2µs    86667.        0B     26.0
#> 18 worst               50         100  19.95µs     21µs    46645.        0B     18.7
#> 19 best               100         100  11.46µs   12.5µs    77275.        0B     30.9
#> 20 worst              100         100  31.01µs   32.3µs    30114.        0B     12.1
```
