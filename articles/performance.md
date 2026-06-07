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
#> 1 foo_S7(x)    7.32µs   9.04µs   103429.    10.8KB     20.7
#> 2 foo_S3(x)     2.5µs   2.91µs   306152.        0B      0  
#> 3 foo_S4(x)    2.71µs   3.31µs   285920.        0B     28.6

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
#> 1 bar_S7(x, y)  12.92µs  15.57µs    61653.        0B     18.5
#> 2 bar_S4(x, y)   6.97µs   8.19µs   117765.        0B     23.6
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
#>  1 best                 3          15   7.51µs   9.48µs   101372.        0B     20.3
#>  2 worst                3          15   7.76µs    9.7µs    97998.        0B     29.4
#>  3 best                 5          15   7.58µs   9.45µs   101398.        0B     30.4
#>  4 worst                5          15   7.79µs   9.69µs    98817.        0B     29.7
#>  5 best                10          15   7.64µs   9.47µs   100605.        0B     30.2
#>  6 worst               10          15   7.99µs   9.88µs    97586.        0B     19.5
#>  7 best                50          15   8.14µs  10.04µs    95341.        0B     28.6
#>  8 worst               50          15   9.73µs  11.59µs    82728.        0B     24.8
#>  9 best               100          15   8.86µs  10.79µs    89074.        0B     26.7
#> 10 worst              100          15  11.99µs  13.85µs    69604.        0B     20.9
#> 11 best                 3         100   7.61µs   9.48µs   101195.        0B     30.4
#> 12 worst                3         100   7.93µs   9.88µs    97249.        0B     29.2
#> 13 best                 5         100   7.67µs   9.61µs    99864.        0B     30.0
#> 14 worst                5         100   8.01µs  10.01µs    95524.        0B     28.7
#> 15 best                10         100   7.64µs   9.68µs    97628.        0B     19.5
#> 16 worst               10         100   8.39µs  10.49µs    90568.        0B     27.2
#> 17 best                50         100    8.1µs  10.21µs    93265.        0B     28.0
#> 18 worst               50         100  13.27µs  15.24µs    63353.        0B     12.7
#> 19 best               100         100   8.86µs  10.95µs    86932.        0B     26.1
#> 20 worst              100         100  19.56µs   21.7µs    44567.        0B     13.4
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
#>  1 best                 3          15   9.35µs   11.8µs    79368.        0B    23.8 
#>  2 worst                3          15    9.7µs   12.1µs    76767.        0B    30.7 
#>  3 best                 5          15   9.43µs   11.9µs    79053.        0B    23.7 
#>  4 worst                5          15   9.89µs   12.5µs    74504.        0B    29.8 
#>  5 best                10          15   9.34µs   11.9µs    78743.        0B    31.5 
#>  6 worst               10          15   9.79µs   11.7µs    80132.        0B    24.0 
#>  7 best                50          15  10.35µs   11.4µs    84312.        0B    33.7 
#>  8 worst               50          15   13.7µs   14.8µs    65424.        0B    19.6 
#>  9 best               100          15  11.68µs   12.9µs    75175.        0B    30.1 
#> 10 worst              100          15  18.41µs   19.7µs    48853.        0B    19.5 
#> 11 best                 3         100   9.59µs   11.2µs    85523.        0B    25.7 
#> 12 worst                3         100  10.42µs     12µs    79561.        0B    31.8 
#> 13 best                 5         100   9.58µs   11.2µs    85601.        0B    25.7 
#> 14 worst                5         100  10.49µs   12.1µs    79098.        0B    31.7 
#> 15 best                10         100   9.38µs   11.1µs    85393.        0B    34.2 
#> 16 worst               10         100  11.56µs   13.2µs    72319.        0B    21.7 
#> 17 best                50         100  10.93µs   12.5µs    75858.        0B    30.4 
#> 18 worst               50         100  20.25µs   21.9µs    44184.        0B    13.3 
#> 19 best               100         100     12µs   13.8µs    69233.        0B    34.6 
#> 20 worst              100         100   31.3µs   33.4µs    29162.        0B     8.75
```
