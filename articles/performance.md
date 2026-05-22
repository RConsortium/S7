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
#> 1 foo_S7(x)    7.11µs      9µs   102689.    18.2KB     20.5
#> 2 foo_S3(x)    2.52µs   2.91µs   310644.        0B      0  
#> 3 foo_S4(x)    2.77µs   3.37µs   284413.        0B     28.4

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
#> 1 bar_S7(x, y)  12.84µs  15.18µs    63895.        0B     19.2
#> 2 bar_S4(x, y)   6.97µs   8.28µs   116741.        0B     23.4
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
#>  1 best                 3          15   7.43µs   9.27µs   104480.        0B     20.9
#>  2 worst                3          15    7.5µs   9.42µs   102321.        0B     30.7
#>  3 best                 5          15   7.35µs   9.35µs   102286.        0B     30.7
#>  4 worst                5          15   7.54µs   9.47µs   101775.        0B     30.5
#>  5 best                10          15   7.33µs   9.25µs   104040.        0B     31.2
#>  6 worst               10          15    7.8µs   9.72µs    99429.        0B     19.9
#>  7 best                50          15   7.89µs   9.86µs    98152.        0B     29.5
#>  8 worst               50          15   9.62µs  11.56µs    83903.        0B     25.2
#>  9 best               100          15   8.65µs  10.64µs    90002.        0B     27.0
#> 10 worst              100          15  11.99µs  13.96µs    69444.        0B     20.8
#> 11 best                 3         100   7.46µs   9.52µs   100449.        0B     30.1
#> 12 worst                3         100   7.45µs    9.4µs   101825.        0B     30.6
#> 13 best                 5         100   7.38µs   9.27µs   103370.        0B     31.0
#> 14 worst                5         100   7.84µs   9.85µs    97735.        0B     29.3
#> 15 best                10         100   7.44µs   9.62µs    97595.        0B     29.3
#> 16 worst               10         100   8.23µs  10.42µs    90724.        0B     27.2
#> 17 best                50         100   7.97µs  10.22µs    93057.        0B     27.9
#> 18 worst               50         100  13.05µs  15.33µs    62710.        0B     12.5
#> 19 best               100         100    8.9µs  11.25µs    84949.        0B     25.5
#> 20 worst              100         100  19.55µs  21.81µs    44336.        0B     13.3
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
#>  1 best                 3          15   9.15µs   11.6µs    81377.        0B    24.4 
#>  2 worst                3          15   9.36µs   11.9µs    79352.        0B    31.8 
#>  3 best                 5          15   9.17µs   11.7µs    81141.        0B    24.3 
#>  4 worst                5          15   9.34µs   11.8µs    79884.        0B    32.0 
#>  5 best                10          15   9.17µs   11.6µs    81072.        0B    32.4 
#>  6 worst               10          15   9.64µs   11.6µs    81108.        0B    24.3 
#>  7 best                50          15  10.42µs   11.4µs    84810.        0B    33.9 
#>  8 worst               50          15  13.45µs   14.5µs    67233.        0B    20.2 
#>  9 best               100          15  11.64µs   12.9µs    75576.        0B    30.2 
#> 10 worst              100          15  18.36µs   19.9µs    48325.        0B    19.3 
#> 11 best                 3         100   9.28µs   11.2µs    83847.        0B    33.6 
#> 12 worst                3         100  10.26µs   12.1µs    78759.        0B    23.6 
#> 13 best                 5         100   9.27µs   11.1µs    84549.        0B    33.8 
#> 14 worst                5         100  10.39µs   12.2µs    78259.        0B    23.5 
#> 15 best                10         100   9.44µs   11.3µs    83307.        0B    25.0 
#> 16 worst               10         100  11.64µs   13.6µs    70335.        0B    21.1 
#> 17 best                50         100   10.7µs   12.5µs    75898.        0B    22.8 
#> 18 worst               50         100  20.25µs   22.3µs    43139.        0B    17.3 
#> 19 best               100         100  11.96µs     14µs    66782.        0B    33.4 
#> 20 worst              100         100  31.32µs   33.4µs    28990.        0B     8.70
```
