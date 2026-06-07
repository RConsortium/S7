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
#> 1 foo_S7(x)    7.37µs   8.79µs   106561.    10.8KB     21.3
#> 2 foo_S3(x)    2.51µs   2.86µs   319616.        0B      0  
#> 3 foo_S4(x)    2.68µs   3.23µs   293440.        0B     29.3

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
#> 1 bar_S7(x, y)   13.1µs  15.18µs    64049.        0B     19.2
#> 2 bar_S4(x, y)    6.9µs   7.99µs   121749.        0B     24.4
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
#>  1 best                 3          15    7.4µs   9.04µs   106972.        0B     21.4
#>  2 worst                3          15   7.64µs   9.17µs   105477.        0B     31.7
#>  3 best                 5          15   7.33µs   8.98µs   106652.        0B     32.0
#>  4 worst                5          15   7.66µs   9.26µs   104335.        0B     31.3
#>  5 best                10          15   7.61µs   9.11µs   105900.        0B     31.8
#>  6 worst               10          15   7.96µs   9.48µs   102491.        0B     20.5
#>  7 best                50          15   8.18µs   9.78µs    99231.        0B     29.8
#>  8 worst               50          15   9.74µs  11.27µs    85958.        0B     25.8
#>  9 best               100          15   8.76µs  10.26µs    94730.        0B     28.4
#> 10 worst              100          15  12.02µs  13.44µs    72622.        0B     21.8
#> 11 best                 3         100   7.47µs   8.97µs   107916.        0B     32.4
#> 12 worst                3         100   7.81µs   9.24µs   104989.        0B     31.5
#> 13 best                 5         100   7.71µs   9.21µs   105240.        0B     31.6
#> 14 worst                5         100   8.04µs   9.56µs   101158.        0B     30.4
#> 15 best                10         100   7.62µs   9.08µs   106067.        0B     21.2
#> 16 worst               10         100   8.26µs   9.67µs    99554.        0B     29.9
#> 17 best                50         100   7.97µs   9.53µs   100928.        0B     30.3
#> 18 worst               50         100  13.19µs  14.71µs    66264.        0B     13.3
#> 19 best               100         100   8.83µs  10.27µs    93965.        0B     28.2
#> 20 worst              100         100   19.5µs   21.1µs    46188.        0B     13.9
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
#>  1 best                 3          15   9.27µs     11µs    87822.        0B     26.4
#>  2 worst                3          15   9.51µs   11.4µs    83724.        0B     33.5
#>  3 best                 5          15   9.38µs     11µs    87064.        0B     26.1
#>  4 worst                5          15   9.63µs   11.4µs    84069.        0B     33.6
#>  5 best                10          15   9.46µs   11.2µs    85496.        0B     34.2
#>  6 worst               10          15  10.04µs   11.2µs    84774.        0B     25.4
#>  7 best                50          15  10.38µs   11.1µs    87652.        0B     35.1
#>  8 worst               50          15  13.58µs   14.4µs    68114.        0B     20.4
#>  9 best               100          15  11.66µs   12.5µs    78225.        0B     31.3
#> 10 worst              100          15  18.34µs   19.4µs    50355.        0B     20.1
#> 11 best                 3         100   9.46µs   10.8µs    88404.        0B     26.5
#> 12 worst                3         100  10.36µs   11.5µs    83732.        0B     33.5
#> 13 best                 5         100   9.36µs   10.7µs    90735.        0B     27.2
#> 14 worst                5         100  10.44µs   11.7µs    82767.        0B     24.8
#> 15 best                10         100   9.54µs   10.8µs    89182.        0B     35.7
#> 16 worst               10         100  11.82µs   13.1µs    74299.        0B     22.3
#> 17 best                50         100  10.87µs   12.2µs    78831.        0B     31.5
#> 18 worst               50         100  20.22µs   21.7µs    44856.        0B     13.5
#> 19 best               100         100  11.97µs   13.3µs    72278.        0B     36.2
#> 20 worst              100         100   31.3µs   33.1µs    29409.        0B     11.8
```
