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
#> 1 foo_S7(x)    6.74µs   8.04µs   117068.    10.8KB     23.4
#> 2 foo_S3(x)    2.28µs   2.56µs   346796.        0B      0  
#> 3 foo_S4(x)     2.5µs    2.9µs   328345.        0B     32.8

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
#> 1 bar_S7(x, y)  11.95µs   13.9µs    69951.        0B     21.0
#> 2 bar_S4(x, y)   6.38µs    7.2µs   135080.        0B     27.0
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
#>  1 best                 3          15   6.85µs   8.13µs   119065.        0B     23.8
#>  2 worst                3          15   6.98µs   8.34µs   116432.        0B     34.9
#>  3 best                 5          15   6.81µs   8.23µs   117276.        0B     35.2
#>  4 worst                5          15   7.01µs   8.45µs   114691.        0B     34.4
#>  5 best                10          15   6.88µs   8.32µs   116379.        0B     34.9
#>  6 worst               10          15   7.19µs    8.5µs   114263.        0B     22.9
#>  7 best                50          15    7.2µs   8.63µs   111893.        0B     33.6
#>  8 worst               50          15   8.58µs  10.12µs    95717.        0B     28.7
#>  9 best               100          15   7.66µs   9.12µs   105862.        0B     42.4
#> 10 worst              100          15  10.68µs  12.19µs    79988.        0B     24.0
#> 11 best                 3         100   6.92µs   8.39µs   115144.        0B     34.6
#> 12 worst                3         100   7.24µs   8.82µs   109964.        0B     33.0
#> 13 best                 5         100   6.87µs   8.42µs   114304.        0B     34.3
#> 14 worst                5         100   7.25µs   8.72µs   110329.        0B     33.1
#> 15 best                10         100   6.93µs   8.41µs   114083.        0B     34.2
#> 16 worst               10         100   7.51µs   9.11µs   105251.        0B     31.6
#> 17 best                50         100   7.17µs   8.85µs   108895.        0B     21.8
#> 18 worst               50         100  11.58µs  13.41µs    72374.        0B     21.7
#> 19 best               100         100   7.86µs   9.47µs   102366.        0B     30.7
#> 20 worst              100         100  16.99µs   18.9µs    51717.        0B     15.5
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
#>  1 best                 3          15   8.51µs  10.37µs    92375.        0B     37.0
#>  2 worst                3          15   8.84µs  10.71µs    89872.        0B     27.0
#>  3 best                 5          15   8.55µs  10.42µs    91926.        0B     27.6
#>  4 worst                5          15   8.88µs  10.99µs    87814.        0B     35.1
#>  5 best                10          15   8.66µs  10.61µs    91168.        0B     27.4
#>  6 worst               10          15    9.2µs  10.04µs    94301.        0B     37.7
#>  7 best                50          15   9.22µs   9.73µs    99615.        0B     39.9
#>  8 worst               50          15  12.01µs  12.54µs    77276.        0B     23.2
#>  9 best               100          15  10.12µs  10.64µs    91347.        0B     36.6
#> 10 worst              100          15  15.89µs  16.81µs    57807.        0B     23.1
#> 11 best                 3         100   8.63µs   9.62µs    98971.        0B     39.6
#> 12 worst                3         100   9.33µs  10.21µs    93946.        0B     28.2
#> 13 best                 5         100   8.51µs   9.48µs   101033.        0B     40.4
#> 14 worst                5         100   9.37µs  10.27µs    93382.        0B     28.0
#> 15 best                10         100   8.54µs   9.42µs   101866.        0B     40.8
#> 16 worst               10         100   10.5µs  11.47µs    83782.        0B     25.1
#> 17 best                50         100   9.45µs  10.31µs    93020.        0B     27.9
#> 18 worst               50         100  17.49µs  18.59µs    52329.        0B     20.9
#> 19 best               100         100  10.31µs   11.1µs    86229.        0B     34.5
#> 20 worst              100         100  26.82µs  28.13µs    34773.        0B     13.9
```
