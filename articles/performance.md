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
#> 1 foo_S7(x)    7.12µs   8.41µs   111190.    18.2KB     22.2
#> 2 foo_S3(x)    2.58µs   2.87µs   319199.        0B      0  
#> 3 foo_S4(x)    2.77µs   3.15µs   305451.        0B     30.5

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
#> 1 bar_S7(x, y)  13.32µs  14.92µs    64243.        0B     25.7
#> 2 bar_S4(x, y)   7.42µs   8.38µs   116088.        0B     23.2
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
#>  1 best                 3          15   7.17µs   8.52µs   113163.        0B     34.0
#>  2 worst                3          15    7.3µs   8.72µs   110694.        0B     22.1
#>  3 best                 5          15   7.12µs   8.52µs   113058.        0B     22.6
#>  4 worst                5          15   7.42µs   8.82µs   110036.        0B     33.0
#>  5 best                10          15   7.27µs    8.6µs   111888.        0B     33.6
#>  6 worst               10          15   7.34µs   8.71µs   108814.        0B     21.8
#>  7 best                50          15   7.76µs   9.04µs   107422.        0B     32.2
#>  8 worst               50          15   9.41µs  10.72µs    90022.        0B     18.0
#>  9 best               100          15    8.3µs   9.71µs    98993.        0B     29.7
#> 10 worst              100          15  11.51µs   12.9µs    74574.        0B     22.4
#> 11 best                 3         100   7.25µs   8.73µs   110140.        0B     33.1
#> 12 worst                3         100    7.7µs   9.06µs   105027.        0B     21.0
#> 13 best                 5         100   7.37µs   8.79µs   109658.        0B     21.9
#> 14 worst                5         100   7.76µs   9.18µs   105265.        0B     31.6
#> 15 best                10         100   7.36µs   8.74µs   109155.        0B     21.8
#> 16 worst               10         100   8.18µs   9.53µs    99610.        0B     29.9
#> 17 best                50         100   7.78µs    9.1µs   105521.        0B     21.1
#> 18 worst               50         100  12.86µs  14.29µs    67087.        0B     20.1
#> 19 best               100         100    8.7µs   10.1µs    94946.        0B     28.5
#> 20 worst              100         100  19.18µs  20.75µs    46033.        0B     13.8
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
#>  1 best                 3          15   8.86µs   10.5µs    90928.        0B     27.3
#>  2 worst                3          15   9.29µs   10.8µs    87406.        0B     35.0
#>  3 best                 5          15   9.09µs   10.5µs    91008.        0B     27.3
#>  4 worst                5          15   9.48µs     11µs    86930.        0B     26.1
#>  5 best                10          15   9.23µs   10.8µs    88788.        0B     26.6
#>  6 worst               10          15     10µs   11.5µs    82979.        0B     24.9
#>  7 best                50          15   9.94µs   10.8µs    87869.        0B     26.4
#>  8 worst               50          15  13.26µs     14µs    69703.        0B     20.9
#>  9 best               100          15  11.37µs   12.2µs    80328.        0B     24.1
#> 10 worst              100          15  17.61µs   18.5µs    52690.        0B     21.1
#> 11 best                 3         100   9.17µs     10µs    96172.        0B     38.5
#> 12 worst                3         100    9.9µs   11.1µs    85358.        0B     34.2
#> 13 best                 5         100   9.16µs   10.3µs    93175.        0B     28.0
#> 14 worst                5         100  10.06µs   11.2µs    85927.        0B     25.8
#> 15 best                10         100   9.17µs   10.2µs    93293.        0B     28.0
#> 16 worst               10         100  11.37µs   12.5µs    76635.        0B     23.0
#> 17 best                50         100  10.52µs   11.7µs    81571.        0B     32.6
#> 18 worst               50         100  19.49µs   20.8µs    46289.        0B     13.9
#> 19 best               100         100  11.67µs   12.8µs    74757.        0B     29.9
#> 20 worst              100         100  30.27µs   31.6µs    30558.        0B     12.2
```
