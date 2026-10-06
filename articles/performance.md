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
#> 1 foo_S7(x)    3.21µs   3.79µs   250845.    10.9KB     50.2
#> 2 foo_S3(x)     1.5µs   1.74µs   521718.        0B     52.2
#> 3 foo_S4(x)    1.61µs   1.93µs   499204.        0B     49.9

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
#> 1 bar_S7(x, y)   7.19µs   8.24µs   117517.        0B     35.3
#> 2 bar_S4(x, y)   4.12µs   4.74µs   205662.        0B     20.6
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
#>  1 best                 3          15   3.42µs   4.13µs   234877.        0B     47.0
#>  2 worst                3          15    3.5µs   4.08µs   238893.        0B     47.8
#>  3 best                 5          15    3.4µs   3.93µs   248280.        0B     49.7
#>  4 worst                5          15   3.56µs   4.26µs   225596.        0B     45.1
#>  5 best                10          15    3.5µs   4.28µs   224930.        0B     45.0
#>  6 worst               10          15   3.75µs   4.44µs   218411.        0B     21.8
#>  7 best                50          15   3.62µs   4.12µs   236397.        0B     47.3
#>  8 worst               50          15   4.68µs   5.13µs   191169.        0B     38.2
#>  9 best               100          15   3.74µs   4.23µs   230574.        0B     46.1
#> 10 worst              100          15   5.98µs    6.5µs   150889.        0B     30.2
#> 11 best                 3         100   3.59µs   4.11µs   235132.        0B     47.0
#> 12 worst                3         100   3.78µs   4.38µs   222561.        0B     44.5
#> 13 best                 5         100    3.5µs   4.02µs   241227.        0B     48.3
#> 14 worst                5         100   3.65µs   4.13µs   236610.        0B     47.3
#> 15 best                10         100   3.37µs   3.82µs   254815.        0B     51.0
#> 16 worst               10         100   3.87µs    4.4µs   222356.        0B     22.2
#> 17 best                50         100   3.56µs   4.03µs   242143.        0B     48.4
#> 18 worst               50         100   6.86µs   7.45µs   130789.        0B     26.2
#> 19 best               100         100   3.83µs   4.45µs   217955.        0B     43.6
#> 20 worst              100         100  10.96µs  11.68µs    84188.        0B     16.8
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
#>  1 best                 3          15   4.66µs   5.44µs   178339.        0B     35.7
#>  2 worst                3          15   4.93µs   5.58µs   175090.        0B     35.0
#>  3 best                 5          15   4.84µs   5.52µs   175607.        0B     52.7
#>  4 worst                5          15   5.06µs   5.63µs   173660.        0B     52.1
#>  5 best                10          15   4.62µs   5.35µs   181348.        0B     36.3
#>  6 worst               10          15   5.11µs   6.02µs   159507.        0B     47.9
#>  7 best                50          15   4.91µs   5.63µs   172618.        0B     51.8
#>  8 worst               50          15   6.95µs   7.64µs   125856.        0B     25.2
#>  9 best               100          15   5.14µs   5.88µs   165137.        0B     49.6
#> 10 worst              100          15   9.28µs  10.25µs    94475.        0B     28.4
#> 11 best                 3         100   4.73µs   5.87µs   161606.        0B     32.3
#> 12 worst                3         100   5.22µs   6.36µs   148500.        0B     44.6
#> 13 best                 5         100   4.64µs      6µs   154577.        0B     46.4
#> 14 worst                5         100   5.66µs   6.79µs   138855.        0B     41.7
#> 15 best                10         100   4.89µs    5.8µs   164510.        0B     49.4
#> 16 worst               10         100   6.15µs   7.04µs   138670.        0B     27.7
#> 17 best                50         100   5.03µs   5.66µs   172314.        0B     34.5
#> 18 worst               50         100   10.8µs  11.53µs    85247.        0B     25.6
#> 19 best               100         100   5.43µs      6µs   162553.        0B     32.5
#> 20 worst              100         100  17.24µs  18.24µs    54218.        0B     16.3
```
