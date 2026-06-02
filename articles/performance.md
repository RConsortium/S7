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
#> 1 foo_S7(x)    6.79µs   8.09µs   115689.    10.8KB     23.1
#> 2 foo_S3(x)    2.32µs   2.67µs   338776.        0B      0  
#> 3 foo_S4(x)    2.48µs   2.82µs   334553.        0B     33.5

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
#> 1 bar_S7(x, y)  12.14µs  14.01µs    69159.        0B     27.7
#> 2 bar_S4(x, y)   6.44µs   7.25µs   134037.        0B     26.8
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
#>  1 best                 3          15   6.84µs   8.25µs   117444.        0B     23.5
#>  2 worst                3          15   7.01µs   8.43µs   114741.        0B     34.4
#>  3 best                 5          15   6.85µs   8.27µs   117166.        0B     35.2
#>  4 worst                5          15   7.09µs   8.49µs   114020.        0B     34.2
#>  5 best                10          15   6.89µs   8.29µs   117006.        0B     35.1
#>  6 worst               10          15   7.19µs   8.56µs   113306.        0B     22.7
#>  7 best                50          15   7.25µs   8.68µs   111298.        0B     33.4
#>  8 worst               50          15    8.7µs  10.27µs    94256.        0B     28.3
#>  9 best               100          15   7.64µs   9.16µs   105616.        0B     31.7
#> 10 worst              100          15  10.64µs  12.25µs    78642.        0B     23.6
#> 11 best                 3         100   6.85µs   8.28µs   116485.        0B     35.0
#> 12 worst                3         100   7.18µs   8.62µs   111918.        0B     33.6
#> 13 best                 5         100   6.97µs   8.37µs   114877.        0B     46.0
#> 14 worst                5         100   7.21µs   8.72µs   110782.        0B     22.2
#> 15 best                10         100   6.92µs   8.49µs   113110.        0B     22.6
#> 16 worst               10         100    7.5µs   9.07µs   105902.        0B     31.8
#> 17 best                50         100   7.26µs   8.86µs   107986.        0B     32.4
#> 18 worst               50         100  11.69µs  13.34µs    73062.        0B     14.6
#> 19 best               100         100   7.83µs   9.37µs   102815.        0B     30.9
#> 20 worst              100         100  16.98µs   18.7µs    52181.        0B     15.7
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
#>  1 best                 3          15   8.43µs  10.25µs    93733.        0B     28.1
#>  2 worst                3          15   8.74µs  10.58µs    90666.        0B     36.3
#>  3 best                 5          15   8.51µs  10.24µs    94008.        0B     28.2
#>  4 worst                5          15   8.91µs  10.73µs    89533.        0B     35.8
#>  5 best                10          15    8.6µs  10.35µs    92831.        0B     37.1
#>  6 worst               10          15    9.2µs   10.4µs    91706.        0B     27.5
#>  7 best                50          15   9.23µs    9.8µs    98415.        0B     39.4
#>  8 worst               50          15  12.04µs  12.71µs    76533.        0B     23.0
#>  9 best               100          15  10.14µs  10.79µs    89681.        0B     35.9
#> 10 worst              100          15  15.88µs  16.72µs    58130.        0B     23.3
#> 11 best                 3         100   8.58µs    9.6µs    99357.        0B     29.8
#> 12 worst                3         100   9.37µs   10.4µs    92254.        0B     36.9
#> 13 best                 5         100   8.49µs   9.39µs   101859.        0B     30.6
#> 14 worst                5         100   9.37µs  10.44µs    92095.        0B     36.9
#> 15 best                10         100    8.5µs   9.46µs   101292.        0B     30.4
#> 16 worst               10         100  10.52µs  11.58µs    83088.        0B     33.2
#> 17 best                50         100   9.54µs  10.53µs    91183.        0B     36.5
#> 18 worst               50         100  17.39µs  18.48µs    52727.        0B     15.8
#> 19 best               100         100  10.25µs  11.19µs    86403.        0B     34.6
#> 20 worst              100         100  26.85µs  27.91µs    35175.        0B     14.1
```
