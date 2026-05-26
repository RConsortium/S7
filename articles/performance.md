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
#> 1 foo_S7(x)    7.53µs   8.89µs   105216.    10.8KB     21.0
#> 2 foo_S3(x)     2.5µs   2.75µs   330225.        0B      0  
#> 3 foo_S4(x)    2.72µs   3.15µs   306503.        0B     30.7

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
#> 1 bar_S7(x, y)  13.68µs  15.57µs    61871.        0B     18.6
#> 2 bar_S4(x, y)   7.24µs   8.26µs   116218.        0B     23.2
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
#>  1 best                 3          15   7.56µs   9.15µs   105462.        0B     21.1
#>  2 worst                3          15   7.86µs   9.38µs   103048.        0B     30.9
#>  3 best                 5          15   7.63µs   9.19µs   104881.        0B     31.5
#>  4 worst                5          15   7.91µs   9.35µs   102876.        0B     30.9
#>  5 best                10          15   7.71µs   9.24µs   104597.        0B     31.4
#>  6 worst               10          15   8.05µs   9.57µs   101023.        0B     20.2
#>  7 best                50          15   8.23µs   9.77µs    98814.        0B     29.7
#>  8 worst               50          15  10.17µs  11.72µs    82609.        0B     24.8
#>  9 best               100          15   8.78µs  10.39µs    92961.        0B     27.9
#> 10 worst              100          15  13.23µs  14.73µs    65751.        0B     19.7
#> 11 best                 3         100   7.61µs   9.21µs   104589.        0B     31.4
#> 12 worst                3         100   7.96µs   9.62µs    99952.        0B     30.0
#> 13 best                 5         100    7.7µs    9.3µs   103108.        0B     41.3
#> 14 worst                5         100   7.96µs   9.59µs   100161.        0B     20.0
#> 15 best                10         100   7.75µs   9.52µs   100637.        0B     20.1
#> 16 worst               10         100   8.35µs  10.23µs    92253.        0B     27.7
#> 17 best                50         100   8.12µs   9.86µs    96949.        0B     29.1
#> 18 worst               50         100  13.71µs  15.37µs    62877.        0B     18.9
#> 19 best               100         100   8.94µs  10.59µs    90675.        0B     27.2
#> 20 worst              100         100  20.36µs  22.03µs    43876.        0B     13.2
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
#>  1 best                 3          15   9.44µs   11.2µs    85287.        0B     25.6
#>  2 worst                3          15   9.87µs   11.6µs    82242.        0B     32.9
#>  3 best                 5          15   9.52µs   11.4µs    83899.        0B     33.6
#>  4 worst                5          15  10.03µs   11.8µs    80847.        0B     24.3
#>  5 best                10          15   9.64µs   11.6µs    81996.        0B     32.8
#>  6 worst               10          15  10.21µs   11.5µs    83042.        0B     24.9
#>  7 best                50          15  10.23µs   11.3µs    84759.        0B     25.4
#>  8 worst               50          15  14.09µs     15µs    64781.        0B     25.9
#>  9 best               100          15  11.57µs   12.5µs    77589.        0B     38.8
#> 10 worst              100          15  19.43µs   20.7µs    46760.        0B     18.7
#> 11 best                 3         100   9.69µs     11µs    86720.        0B     26.0
#> 12 worst                3         100  10.54µs   11.9µs    80400.        0B     32.2
#> 13 best                 5         100   9.46µs   10.9µs    87810.        0B     26.4
#> 14 worst                5         100   10.6µs     12µs    79019.        0B     31.6
#> 15 best                10         100   9.72µs     11µs    86669.        0B     26.0
#> 16 worst               10         100  11.97µs   13.4µs    71250.        0B     28.5
#> 17 best                50         100   10.9µs   12.4µs    76991.        0B     30.8
#> 18 worst               50         100  20.63µs   22.2µs    43562.        0B     13.1
#> 19 best               100         100  12.12µs   13.7µs    69670.        0B     27.9
#> 20 worst              100         100  32.63µs   34.6µs    27944.        0B     11.2
```
