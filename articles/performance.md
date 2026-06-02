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
#> 1 foo_S7(x)    7.69µs   8.95µs   105022.    10.8KB     21.0
#> 2 foo_S3(x)    2.57µs   2.79µs   326101.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.09µs   311766.        0B     31.2

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
#> 1 bar_S7(x, y)  13.74µs   15.5µs    62375.        0B     25.0
#> 2 bar_S4(x, y)   7.26µs    8.1µs   120096.        0B     24.0
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
#>  1 best                 3          15   7.72µs    9.2µs   105468.        0B     21.1
#>  2 worst                3          15   7.87µs    9.2µs   105580.        0B     31.7
#>  3 best                 5          15   7.75µs   9.16µs   105767.        0B     31.7
#>  4 worst                5          15   8.11µs   9.45µs   102596.        0B     30.8
#>  5 best                10          15   7.71µs   9.16µs   105722.        0B     31.7
#>  6 worst               10          15   8.18µs   9.74µs    99259.        0B     19.9
#>  7 best                50          15   8.12µs   9.68µs    99512.        0B     29.9
#>  8 worst               50          15  10.14µs  11.64µs    82302.        0B     24.7
#>  9 best               100          15   8.83µs  10.33µs    93052.        0B     27.9
#> 10 worst              100          15  12.77µs  14.34µs    66854.        0B     20.1
#> 11 best                 3         100   7.72µs   9.18µs   104830.        0B     31.5
#> 12 worst                3         100   8.15µs   9.55µs   100828.        0B     30.3
#> 13 best                 5         100    7.8µs   9.33µs   102633.        0B     30.8
#> 14 worst                5         100   8.22µs   9.72µs    98384.        0B     29.5
#> 15 best                10         100   7.76µs   9.29µs   102637.        0B     20.5
#> 16 worst               10         100   8.64µs  10.05µs    95031.        0B     28.5
#> 17 best                50         100   8.21µs   9.67µs    98933.        0B     29.7
#> 18 worst               50         100  13.79µs  15.26µs    63409.        0B     12.7
#> 19 best               100         100      9µs  10.56µs    90259.        0B     27.1
#> 20 worst              100         100  20.39µs  21.92µs    44142.        0B     13.2
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
#>  1 best                 3          15   9.62µs   11.5µs    82964.        0B     24.9
#>  2 worst                3          15   9.88µs   11.8µs    80262.        0B     32.1
#>  3 best                 5          15    9.6µs   11.4µs    83393.        0B     25.0
#>  4 worst                5          15  10.17µs     12µs    79251.        0B     31.7
#>  5 best                10          15   9.78µs   11.5µs    82842.        0B     33.2
#>  6 worst               10          15  10.42µs   11.8µs    81316.        0B     24.4
#>  7 best                50          15  10.45µs   11.3µs    85247.        0B     34.1
#>  8 worst               50          15  14.08µs   14.9µs    65406.        0B     19.6
#>  9 best               100          15  11.75µs   12.6µs    76628.        0B     30.7
#> 10 worst              100          15  19.41µs   20.6µs    46115.        0B     18.5
#> 11 best                 3         100   9.83µs     11µs    87575.        0B     26.3
#> 12 worst                3         100  10.61µs   11.9µs    81303.        0B     32.5
#> 13 best                 5         100   9.61µs   10.9µs    85766.        0B     25.7
#> 14 worst                5         100   10.7µs   11.8µs    81704.        0B     32.7
#> 15 best                10         100   9.83µs     11µs    87670.        0B     26.3
#> 16 worst               10         100  12.01µs   13.3µs    72702.        0B     29.1
#> 17 best                50         100  10.95µs   12.2µs    78988.        0B     31.6
#> 18 worst               50         100  20.84µs   22.1µs    43902.        0B     13.2
#> 19 best               100         100  11.98µs   13.2µs    73196.        0B     29.3
#> 20 worst              100         100  32.52µs     34µs    28597.        0B     11.4
```
