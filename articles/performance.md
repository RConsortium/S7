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
#> 1 foo_S7(x)    7.36µs   8.98µs   104068.    10.8KB     20.8
#> 2 foo_S3(x)    2.51µs   2.89µs   315253.        0B      0  
#> 3 foo_S4(x)    2.74µs   3.29µs   292155.        0B     29.2

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
#> 1 bar_S7(x, y)  13.15µs  15.18µs    63617.        0B     19.1
#> 2 bar_S4(x, y)   6.95µs   8.05µs   120974.        0B     24.2
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
#>  1 best                 3          15   7.44µs   9.11µs   106194.        0B     21.2
#>  2 worst                3          15   7.66µs   9.29µs   104551.        0B     31.4
#>  3 best                 5          15   7.57µs   9.19µs   105196.        0B     31.6
#>  4 worst                5          15   7.75µs   9.48µs   101975.        0B     30.6
#>  5 best                10          15   7.54µs   9.22µs   105254.        0B     31.6
#>  6 worst               10          15   7.74µs   9.35µs   103856.        0B     20.8
#>  7 best                50          15   8.03µs   9.72µs    99836.        0B     30.0
#>  8 worst               50          15   9.65µs  11.36µs    85444.        0B     25.6
#>  9 best               100          15   8.93µs  10.62µs    91333.        0B     27.4
#> 10 worst              100          15  12.12µs  13.95µs    69650.        0B     20.9
#> 11 best                 3         100   7.71µs   9.46µs   102517.        0B     30.8
#> 12 worst                3         100   8.01µs   9.71µs    99630.        0B     29.9
#> 13 best                 5         100   7.19µs   9.41µs    93398.        0B     28.0
#> 14 worst                5         100   7.86µs   9.65µs    99758.        0B     29.9
#> 15 best                10         100   7.59µs   9.33µs   102785.        0B     30.8
#> 16 worst               10         100   8.31µs  10.09µs    95392.        0B     28.6
#> 17 best                50         100   8.08µs   9.81µs    98050.        0B     29.4
#> 18 worst               50         100  13.27µs  15.02µs    64542.        0B     19.4
#> 19 best               100         100   8.78µs  10.64µs    90458.        0B     36.2
#> 20 worst              100         100  19.53µs  21.42µs    45488.        0B     13.7
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
#>  1 best                 3          15   9.36µs   11.2µs    84663.        0B     33.9
#>  2 worst                3          15   9.71µs   11.6µs    82890.        0B     24.9
#>  3 best                 5          15   9.29µs   11.2µs    85479.        0B     34.2
#>  4 worst                5          15    9.6µs   11.5µs    83848.        0B     25.2
#>  5 best                10          15   9.62µs   11.4µs    84431.        0B     25.3
#>  6 worst               10          15  10.07µs   11.2µs    85414.        0B     34.2
#>  7 best                50          15  10.36µs   11.2µs    86693.        0B     26.0
#>  8 worst               50          15  13.84µs   14.6µs    66638.        0B     26.7
#>  9 best               100          15  11.63µs   12.5µs    77970.        0B     31.2
#> 10 worst              100          15   18.3µs   19.8µs    48663.        0B     19.5
#> 11 best                 3         100   9.45µs   11.1µs    85494.        0B     25.7
#> 12 worst                3         100  10.53µs   12.2µs    77346.        0B     31.0
#> 13 best                 5         100   9.46µs   11.1µs    85058.        0B     34.0
#> 14 worst                5         100   10.6µs   12.3µs    76377.        0B     22.9
#> 15 best                10         100   9.75µs   11.5µs    82200.        0B     32.9
#> 16 worst               10         100  11.97µs   13.5µs    70369.        0B     21.1
#> 17 best                50         100  10.68µs   12.6µs    74786.        0B     29.9
#> 18 worst               50         100  20.15µs   21.9µs    43966.        0B     17.6
#> 19 best               100         100  12.06µs   13.7µs    68549.        0B     27.4
#> 20 worst              100         100  31.32µs   33.6µs    28866.        0B     11.6
```
