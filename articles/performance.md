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
#> 1 foo_S7(x)    6.73µs   8.22µs   114289.    10.8KB     22.9
#> 2 foo_S3(x)    2.29µs   2.64µs   340705.        0B      0  
#> 3 foo_S4(x)    2.47µs   2.83µs   329614.        0B     33.0

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
#> 1 bar_S7(x, y)  11.99µs  13.98µs    69265.        0B     20.8
#> 2 bar_S4(x, y)   6.49µs   7.39µs   131287.        0B     26.3
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
#>  1 best                 3          15   6.79µs   8.28µs   116773.        0B     23.4
#>  2 worst                3          15   7.04µs    8.5µs   113695.        0B     34.1
#>  3 best                 5          15    6.8µs   8.31µs   116534.        0B     35.0
#>  4 worst                5          15   7.01µs   8.58µs   112642.        0B     33.8
#>  5 best                10          15   6.85µs    8.4µs   115535.        0B     34.7
#>  6 worst               10          15   7.16µs   8.69µs   111752.        0B     22.4
#>  7 best                50          15   7.13µs   8.76µs   110391.        0B     33.1
#>  8 worst               50          15   8.61µs  10.22µs    94776.        0B     28.4
#>  9 best               100          15   7.66µs   9.25µs   104615.        0B     31.4
#> 10 worst              100          15  10.55µs  12.28µs    79281.        0B     23.8
#> 11 best                 3         100   6.88µs   8.46µs   113938.        0B     34.2
#> 12 worst                3         100   7.14µs   8.72µs   111014.        0B     33.3
#> 13 best                 5         100   6.93µs   8.55µs   112806.        0B     33.9
#> 14 worst                5         100   7.21µs   8.82µs   109008.        0B     32.7
#> 15 best                10         100   6.89µs   8.47µs   113219.        0B     34.0
#> 16 worst               10         100   7.48µs   9.07µs   106541.        0B     32.0
#> 17 best                50         100   7.17µs   8.84µs   108639.        0B     32.6
#> 18 worst               50         100  11.54µs  13.41µs    72544.        0B     21.8
#> 19 best               100         100   7.81µs   9.54µs   100612.        0B     30.2
#> 20 worst              100         100  16.98µs  18.83µs    51873.        0B     15.6
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
#>  1 best                 3          15   8.41µs  10.36µs    92084.        0B     36.8
#>  2 worst                3          15    8.7µs  10.59µs    90790.        0B     27.2
#>  3 best                 5          15   8.47µs  10.21µs    94110.        0B     28.2
#>  4 worst                5          15   8.85µs   10.8µs    88861.        0B     35.6
#>  5 best                10          15   8.54µs  10.44µs    91696.        0B     27.5
#>  6 worst               10          15   9.25µs  10.06µs    94028.        0B     37.6
#>  7 best                50          15   9.21µs   9.79µs    98849.        0B     39.6
#>  8 worst               50          15  12.04µs  12.66µs    77024.        0B     23.1
#>  9 best               100          15  10.11µs  10.71µs    90703.        0B     36.3
#> 10 worst              100          15  15.86µs  16.83µs    57635.        0B     17.3
#> 11 best                 3         100   8.55µs   9.59µs    99312.        0B     39.7
#> 12 worst                3         100   9.27µs  10.34µs    92332.        0B     27.7
#> 13 best                 5         100   8.46µs   9.38µs   101915.        0B     30.6
#> 14 worst                5         100   9.37µs   10.4µs    92293.        0B     27.7
#> 15 best                10         100   8.49µs   9.47µs   100906.        0B     30.3
#> 16 worst               10         100  10.45µs   11.6µs    82485.        0B     33.0
#> 17 best                50         100   9.36µs  10.44µs    91314.        0B     36.5
#> 18 worst               50         100  17.45µs  18.55µs    52449.        0B     15.7
#> 19 best               100         100  10.33µs  11.36µs    84537.        0B     33.8
#> 20 worst              100         100  26.85µs  28.17µs    34773.        0B     13.9
```
