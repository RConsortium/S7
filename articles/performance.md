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
#> 1 foo_S7(x)    7.11µs   8.72µs   107353.    18.2KB     21.5
#> 2 foo_S3(x)    2.48µs   2.85µs   319126.        0B      0  
#> 3 foo_S4(x)    2.71µs   3.23µs   296859.        0B     29.7

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
#> 1 bar_S7(x, y)  12.95µs  15.11µs    64172.        0B     19.3
#> 2 bar_S4(x, y)   6.91µs   8.05µs   120918.        0B     24.2
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
#>  1 best                 3          15   7.21µs    8.9µs   108728.        0B     21.7
#>  2 worst                3          15   7.44µs   9.23µs   104631.        0B     31.4
#>  3 best                 5          15   7.38µs   9.19µs   104821.        0B     31.5
#>  4 worst                5          15   7.53µs   9.36µs   103048.        0B     30.9
#>  5 best                10          15   7.45µs   9.25µs   104561.        0B     31.4
#>  6 worst               10          15   7.63µs   9.45µs   102605.        0B     20.5
#>  7 best                50          15   7.98µs   9.75µs    99339.        0B     29.8
#>  8 worst               50          15   9.61µs  11.73µs    61123.        0B     18.3
#>  9 best               100          15   8.64µs   10.6µs    85786.        0B     25.7
#> 10 worst              100          15  11.84µs  14.32µs    56685.        0B     17.0
#> 11 best                 3         100   7.36µs  20.65µs    50581.        0B     15.2
#> 12 worst                3         100   7.63µs   9.28µs   103605.        0B     31.1
#> 13 best                 5         100   7.46µs   9.13µs   105332.        0B     42.1
#> 14 worst                5         100   7.78µs   9.45µs   102281.        0B     20.5
#> 15 best                10         100   7.38µs   9.37µs   102537.        0B     20.5
#> 16 worst               10         100   8.12µs   9.88µs    96780.        0B     29.0
#> 17 best                50         100   7.95µs   9.53µs   100790.        0B     30.2
#> 18 worst               50         100  13.24µs  14.97µs    64669.        0B     12.9
#> 19 best               100         100   8.73µs  10.58µs    90138.        0B     27.0
#> 20 worst              100         100  19.52µs  21.49µs    44904.        0B     13.5
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
#>  1 best                 3          15   8.92µs   11.2µs    85028.        0B    25.5 
#>  2 worst                3          15    9.5µs   11.6µs    81570.        0B    32.6 
#>  3 best                 5          15    9.1µs   11.3µs    84410.        0B    25.3 
#>  4 worst                5          15   9.47µs   11.8µs    80077.        0B    32.0 
#>  5 best                10          15   9.48µs   11.5µs    82274.        0B    32.9 
#>  6 worst               10          15   9.87µs   11.4µs    83137.        0B    24.9 
#>  7 best                50          15  10.31µs   11.3µs    86030.        0B    34.4 
#>  8 worst               50          15  13.51µs   14.4µs    66648.        0B    20.0 
#>  9 best               100          15   11.6µs   12.5µs    77452.        0B    31.0 
#> 10 worst              100          15  17.91µs   19.3µs    50283.        0B    20.1 
#> 11 best                 3         100   9.19µs     11µs    86996.        0B    34.8 
#> 12 worst                3         100  10.12µs   11.7µs    81440.        0B    24.4 
#> 13 best                 5         100   9.08µs   10.8µs    88663.        0B    35.5 
#> 14 worst                5         100  10.18µs   11.8µs    81378.        0B    24.4 
#> 15 best                10         100   9.23µs     11µs    87445.        0B    35.0 
#> 16 worst               10         100  11.58µs   13.1µs    73341.        0B    22.0 
#> 17 best                50         100  10.49µs   12.1µs    78786.        0B    31.5 
#> 18 worst               50         100     20µs   21.7µs    44723.        0B    13.4 
#> 19 best               100         100  11.74µs   13.4µs    71448.        0B    35.7 
#> 20 worst              100         100  31.14µs   33.1µs    29478.        0B     8.85
```
