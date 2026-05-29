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
#> 1 foo_S7(x)    7.23µs   8.83µs   105642.    10.8KB     21.1
#> 2 foo_S3(x)    2.49µs   2.89µs   315349.        0B      0  
#> 3 foo_S4(x)     2.7µs   3.23µs   297217.        0B     29.7

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
#> 1 bar_S7(x, y)  13.03µs  15.22µs    63680.        0B     19.1
#> 2 bar_S4(x, y)   6.97µs   8.06µs   119822.        0B     24.0
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
#>  1 best                 3          15   7.44µs   9.18µs   105215.        0B     21.0
#>  2 worst                3          15   7.43µs   9.32µs   103546.        0B     31.1
#>  3 best                 5          15   7.35µs   9.13µs   105562.        0B     31.7
#>  4 worst                5          15   7.44µs   9.29µs   103839.        0B     31.2
#>  5 best                10          15   7.47µs   9.32µs   103809.        0B     31.2
#>  6 worst               10          15   7.55µs   9.44µs   100388.        0B     20.1
#>  7 best                50          15   7.87µs   9.78µs    96638.        0B     29.0
#>  8 worst               50          15   9.68µs  11.46µs    84466.        0B     25.3
#>  9 best               100          15   8.65µs  10.49µs    92110.        0B     27.6
#> 10 worst              100          15  11.63µs  13.72µs    70760.        0B     21.2
#> 11 best                 3         100   7.47µs   9.29µs   103925.        0B     31.2
#> 12 worst                3         100   7.74µs   9.59µs   100301.        0B     40.1
#> 13 best                 5         100   7.41µs   9.38µs   102580.        0B     30.8
#> 14 worst                5         100    7.9µs   9.88µs    96881.        0B     29.1
#> 15 best                10         100   7.54µs   9.46µs   100631.        0B     30.2
#> 16 worst               10         100   8.21µs  10.31µs    92170.        0B     18.4
#> 17 best                50         100   8.03µs   9.98µs    95437.        0B     28.6
#> 18 worst               50         100  13.09µs  15.14µs    63574.        0B     19.1
#> 19 best               100         100   8.63µs  10.68µs    88430.        0B     26.5
#> 20 worst              100         100  19.44µs  21.44µs    45196.        0B     13.6
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
#>  1 best                 3          15   9.19µs   11.5µs    82456.        0B     33.0
#>  2 worst                3          15   9.48µs   11.9µs    79592.        0B     23.9
#>  3 best                 5          15    9.4µs   11.8µs    80410.        0B     32.2
#>  4 worst                5          15   9.71µs   12.2µs    77477.        0B     23.3
#>  5 best                10          15   9.45µs   11.9µs    79113.        0B     31.7
#>  6 worst               10          15    9.7µs   11.3µs    84076.        0B     25.2
#>  7 best                50          15  10.45µs   11.4µs    85754.        0B     25.7
#>  8 worst               50          15  13.57µs   14.5µs    67064.        0B     26.8
#>  9 best               100          15  11.59µs   12.5µs    78012.        0B     39.0
#> 10 worst              100          15   18.1µs   19.6µs    49313.        0B     19.7
#> 11 best                 3         100   9.33µs   11.1µs    86314.        0B     25.9
#> 12 worst                3         100  10.27µs   12.1µs    78905.        0B     31.6
#> 13 best                 5         100   9.23µs   10.9µs    87576.        0B     26.3
#> 14 worst                5         100  10.22µs   12.2µs    78522.        0B     31.4
#> 15 best                10         100   9.43µs   11.3µs    84688.        0B     25.4
#> 16 worst               10         100  11.73µs   13.7µs    70299.        0B     28.1
#> 17 best                50         100   10.8µs   12.5µs    76359.        0B     30.6
#> 18 worst               50         100  19.94µs   21.9µs    44184.        0B     13.3
#> 19 best               100         100  11.96µs   13.4µs    71833.        0B     28.7
#> 20 worst              100         100  31.61µs   33.5µs    29150.        0B     11.7
```
