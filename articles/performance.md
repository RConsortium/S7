# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text := new_class(parent = class_character)
Number := new_class(parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 := new_generic("x")
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
#> 1 foo_S7(x)    3.29µs   4.21µs   214140.    10.9KB     42.8
#> 2 foo_S3(x)    1.32µs   1.68µs   498395.        0B     49.8
#> 3 foo_S4(x)    1.45µs   1.93µs   471336.        0B     47.1

bar_S7 := new_generic(c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)   6.75µs    7.5µs   125424.        0B     37.6
#> 2 bar_S4(x, y)   3.89µs   4.31µs   224433.        0B     44.9
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
    Text := new_class(parent = class_character)
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
    foo_S7 := new_generic("x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic("x")
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
#>  1 best                 3          15   3.29µs   3.65µs   264517.        0B     26.5
#>  2 worst                3          15   3.42µs   3.74µs   256816.        0B     51.4
#>  3 best                 5          15   3.29µs   3.65µs   264917.        0B     53.0
#>  4 worst                5          15   3.47µs   3.81µs   254957.        0B     51.0
#>  5 best                10          15   3.31µs   3.63µs   267519.        0B     53.5
#>  6 worst               10          15   3.57µs   3.91µs   248305.        0B     49.7
#>  7 best                50          15   3.42µs   3.78µs   256321.        0B     51.3
#>  8 worst               50          15   4.54µs   4.86µs   200275.        0B     40.1
#>  9 best               100          15   3.56µs   3.89µs   248466.        0B     49.7
#> 10 worst              100          15   5.64µs   6.05µs   156499.        0B     31.3
#> 11 best                 3         100    3.4µs   3.88µs   243724.        0B     48.8
#> 12 worst                3         100   3.68µs   4.58µs   205534.        0B     41.1
#> 13 best                 5         100   3.91µs   4.31µs   211312.        0B     21.1
#> 14 worst                5         100   4.27µs   4.64µs   209066.        0B     41.8
#> 15 best                10         100   3.91µs   4.27µs   227573.        0B     45.5
#> 16 worst               10         100   4.51µs   4.86µs   200070.        0B     40.0
#> 17 best                50         100   3.99µs   4.36µs   223151.        0B     22.3
#> 18 worst               50         100   8.09µs   8.65µs   113569.        0B     22.7
#> 19 best               100         100   4.25µs   4.64µs   209139.        0B     20.9
#> 20 worst              100         100  13.16µs   13.6µs    72379.        0B     14.5
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text := new_class(parent = class_character)
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
    foo_S7 := new_generic(c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic(c("x", "y"))
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
#>  1 best                 3          15   5.36µs    5.8µs   166744.        0B     50.0
#>  2 worst                3          15   4.83µs   5.53µs   171862.        0B     34.4
#>  3 best                 5          15   4.56µs   4.99µs   192858.        0B     57.9
#>  4 worst                5          15   4.98µs   5.39µs   178719.        0B     53.6
#>  5 best                10          15   4.58µs   5.03µs   191752.        0B     38.4
#>  6 worst               10          15   5.21µs   5.61µs   171848.        0B     51.6
#>  7 best                50          15   4.87µs   5.31µs   182212.        0B     36.4
#>  8 worst               50          15   7.04µs   7.45µs   129972.        0B     39.0
#>  9 best               100          15    5.2µs   5.64µs   171822.        0B     34.4
#> 10 worst              100          15   9.45µs   9.88µs    98589.        0B     29.6
#> 11 best                 3         100   4.73µs   5.16µs   187482.        0B     56.3
#> 12 worst                3         100   5.34µs   5.77µs   166966.        0B     33.4
#> 13 best                 5         100   4.58µs   5.03µs   190960.        0B     57.3
#> 14 worst                5         100   5.41µs    5.9µs   164194.        0B     32.8
#> 15 best                10         100   4.64µs   5.06µs   190789.        0B     38.2
#> 16 worst               10         100    6.3µs   6.59µs   145971.        0B     43.8
#> 17 best                50         100   5.03µs   5.34µs   182266.        0B     36.5
#> 18 worst               50         100  11.61µs     12µs    82067.        0B     24.6
#> 19 best               100         100   5.24µs   5.53µs   176390.        0B     52.9
#> 20 worst              100         100  18.74µs  19.15µs    51530.        0B     15.5
```
