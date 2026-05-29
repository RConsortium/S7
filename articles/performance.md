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
#> 1 foo_S7(x)    3.96µs    4.8µs   193987.    10.8KB     38.8
#> 2 foo_S3(x)     1.5µs   1.66µs   546717.        0B      0  
#> 3 foo_S4(x)    1.55µs   1.78µs   538850.        0B     53.9

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
#> 1 bar_S7(x, y)   6.97µs   8.17µs   119397.        0B     47.8
#> 2 bar_S4(x, y)   3.88µs   4.32µs   226641.        0B     45.3
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
#>  1 best                 3          15   3.89µs   4.84µs   199258.        0B     39.9
#>  2 worst                3          15   4.02µs   4.77µs   201211.        0B     60.4
#>  3 best                 5          15   3.91µs   4.68µs   206389.        0B     61.9
#>  4 worst                5          15   4.04µs   4.72µs   204386.        0B     61.3
#>  5 best                10          15   3.97µs   4.73µs   201662.        0B     60.5
#>  6 worst               10          15   4.19µs   4.82µs   201699.        0B     40.3
#>  7 best                50          15   4.27µs   4.96µs   196769.        0B     59.0
#>  8 worst               50          15   5.24µs   5.94µs   162330.        0B     48.7
#>  9 best               100          15   4.63µs   5.43µs   179651.        0B     53.9
#> 10 worst              100          15   6.66µs    7.3µs   133853.        0B     40.2
#> 11 best                 3         100      4µs   4.65µs   207841.        0B     62.4
#> 12 worst                3         100   4.19µs   4.75µs   205386.        0B     61.6
#> 13 best                 5         100   4.04µs   4.71µs   205369.        0B     82.2
#> 14 worst                5         100   4.27µs   4.92µs   196784.        0B     39.4
#> 15 best                10         100   4.08µs   4.86µs   199679.        0B     39.9
#> 16 worst               10         100   4.52µs   5.23µs   185178.        0B     55.6
#> 17 best                50         100    4.3µs   5.11µs   189008.        0B     56.7
#> 18 worst               50         100   7.43µs   8.11µs   120424.        0B     24.1
#> 19 best               100         100   4.72µs   5.55µs   174468.        0B     52.4
#> 20 worst              100         100  11.37µs  12.19µs    79993.        0B     24.0
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
#>  1 best                 3          15   4.96µs   6.16µs   153812.        0B     46.2
#>  2 worst                3          15   5.16µs   6.12µs   157354.        0B     63.0
#>  3 best                 5          15      5µs   5.94µs   160271.        0B     64.1
#>  4 worst                5          15   5.22µs   6.15µs   156289.        0B     46.9
#>  5 best                10          15   5.11µs    6.1µs   156709.        0B     62.7
#>  6 worst               10          15   5.47µs   6.27µs   155284.        0B     46.6
#>  7 best                50          15   5.65µs   6.27µs   155605.        0B     62.3
#>  8 worst               50          15   7.57µs   8.08µs   120666.        0B     48.3
#>  9 best               100          15   6.41µs   6.94µs   140862.        0B     70.5
#> 10 worst              100          15  10.41µs  11.25µs    85807.        0B     34.3
#> 11 best                 3         100   5.19µs   6.14µs   155319.        0B     46.6
#> 12 worst                3         100   5.57µs    6.5µs   147833.        0B     59.2
#> 13 best                 5         100   4.99µs   5.88µs   161546.        0B     48.5
#> 14 worst                5         100   5.62µs   6.46µs   148530.        0B     59.4
#> 15 best                10         100   5.06µs   5.91µs   162724.        0B     48.8
#> 16 worst               10         100   6.47µs   7.16µs   135772.        0B     54.3
#> 17 best                50         100   5.96µs   6.83µs   141857.        0B     56.8
#> 18 worst               50         100  11.59µs  12.32µs    79517.        0B     23.9
#> 19 best               100         100   6.58µs   7.32µs   132581.        0B     53.1
#> 20 worst              100         100  18.46µs   19.3µs    50892.        0B     20.4
```
