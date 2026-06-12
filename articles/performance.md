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
#> 1 foo_S7(x)    6.72µs   8.28µs   113838.    10.8KB     34.2
#> 2 foo_S3(x)     2.3µs   2.58µs   341907.        0B     34.2
#> 3 foo_S4(x)    2.52µs   2.86µs   325998.        0B     32.6

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
#> 1 bar_S7(x, y)   11.9µs  14.13µs    68478.        0B     27.4
#> 2 bar_S4(x, y)   6.44µs   7.39µs   131449.        0B     26.3
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
#>  1 best                 3          15   6.86µs   8.48µs   113698.        0B     34.1
#>  2 worst                3          15   6.92µs   8.63µs   112120.        0B     33.6
#>  3 best                 5          15   6.76µs   8.46µs   114323.        0B     34.3
#>  4 worst                5          15   7.05µs   8.76µs   110445.        0B     33.1
#>  5 best                10          15   6.79µs   8.48µs   114366.        0B     34.3
#>  6 worst               10          15   7.16µs   8.82µs   109522.        0B     32.9
#>  7 best                50          15   7.18µs   8.89µs   103699.        0B     31.1
#>  8 worst               50          15   8.61µs  10.36µs    93055.        0B     27.9
#>  9 best               100          15   7.57µs   9.32µs   103446.        0B     41.4
#> 10 worst              100          15  10.59µs  12.33µs    78600.        0B     23.6
#> 11 best                 3         100   6.88µs   8.55µs   112038.        0B     44.8
#> 12 worst                3         100   7.17µs   8.87µs   108873.        0B     32.7
#> 13 best                 5         100   6.86µs    8.6µs   111647.        0B     33.5
#> 14 worst                5         100   7.13µs   8.93µs   106955.        0B     32.1
#> 15 best                10         100   6.88µs   8.62µs   110856.        0B     33.3
#> 16 worst               10         100   7.46µs   9.25µs   103626.        0B     31.1
#> 17 best                50         100   7.19µs   8.96µs   106948.        0B     32.1
#> 18 worst               50         100  11.58µs  13.45µs    72186.        0B     21.7
#> 19 best               100         100   7.74µs   9.74µs    98070.        0B     29.4
#> 20 worst              100         100  17.01µs  19.02µs    51127.        0B     15.3
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
#>  1 best                 3          15   8.44µs  10.76µs    89205.        0B     35.7
#>  2 worst                3          15    8.7µs  11.07µs    86256.        0B     25.9
#>  3 best                 5          15   8.44µs  10.73µs    88784.        0B     35.5
#>  4 worst                5          15   8.88µs  10.94µs    87378.        0B     35.0
#>  5 best                10          15   8.45µs   9.02µs   107156.        0B     42.9
#>  6 worst               10          15   9.16µs   9.76µs    98291.        0B     39.3
#>  7 best                50          15   9.17µs   9.73µs    99290.        0B     39.7
#>  8 worst               50          15  11.96µs  12.62µs    76069.        0B     30.4
#>  9 best               100          15  10.17µs  11.14µs    85280.        0B     34.1
#> 10 worst              100          15  15.86µs  16.98µs    56711.        0B     22.7
#> 11 best                 3         100   8.56µs   9.53µs    99456.        0B     39.8
#> 12 worst                3         100   9.28µs  10.34µs    91581.        0B     36.6
#> 13 best                 5         100   8.45µs   9.47µs   100088.        0B     40.1
#> 14 worst                5         100   9.34µs  10.45µs    90311.        0B     36.1
#> 15 best                10         100   8.47µs   9.55µs    98376.        0B     39.4
#> 16 worst               10         100  10.42µs  11.55µs    82200.        0B     32.9
#> 17 best                50         100   9.44µs  10.54µs    89934.        0B     36.0
#> 18 worst               50         100  17.48µs   18.7µs    51385.        0B     20.6
#> 19 best               100         100  10.27µs  11.39µs    83210.        0B     41.6
#> 20 worst              100         100  26.89µs  28.14µs    34564.        0B     13.8
```
