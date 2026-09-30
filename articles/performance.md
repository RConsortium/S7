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
#> 1 foo_S7(x)    5.78µs   6.83µs   136906.    10.9KB     27.4
#> 2 foo_S3(x)     2.5µs   2.93µs   309060.        0B     30.9
#> 3 foo_S4(x)    2.67µs    3.2µs   299348.        0B     29.9

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
#> 1 bar_S7(x, y)  11.86µs  13.63µs    70452.        0B     21.1
#> 2 bar_S4(x, y)   6.86µs   8.16µs   119199.        0B     23.8
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
#>  1 best                 3          15   5.87µs   6.87µs   140534.        0B    28.1 
#>  2 worst                3          15   6.03µs   7.07µs   136115.        0B    27.2 
#>  3 best                 5          15   5.86µs   6.91µs   138871.        0B    27.8 
#>  4 worst                5          15   6.09µs   7.17µs   132598.        0B    26.5 
#>  5 best                10          15   5.93µs   7.25µs   106536.        0B    21.3 
#>  6 worst               10          15   6.28µs    8.4µs    96828.        0B     9.68
#>  7 best                50          15   6.13µs   7.14µs   135067.        0B    27.0 
#>  8 worst               50          15   7.84µs   8.86µs   109384.        0B    10.9 
#>  9 best               100          15    6.4µs   7.44µs   129053.        0B    25.8 
#> 10 worst              100          15   9.63µs  10.74µs    90878.        0B     9.09
#> 11 best                 3         100   5.97µs   7.05µs   136528.        0B    27.3 
#> 12 worst                3         100    6.3µs   7.42µs   129411.        0B    25.9 
#> 13 best                 5         100   6.03µs   7.15µs   133854.        0B    26.8 
#> 14 worst                5         100   6.45µs   7.42µs   128778.        0B    25.8 
#> 15 best                10         100   6.01µs   7.03µs   135756.        0B    27.2 
#> 16 worst               10         100   6.73µs   7.77µs   123776.        0B    12.4 
#> 17 best                50         100   6.08µs   7.08µs   134756.        0B    27.0 
#> 18 worst               50         100  11.19µs   12.3µs    79175.        0B     7.92
#> 19 best               100         100   6.46µs   7.52µs   127058.        0B    25.4 
#> 20 worst              100         100  17.16µs  18.33µs    53346.        0B     5.34
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
#>  1 best                 3          15    8.1µs   9.67µs    98702.        0B     29.6
#>  2 worst                3          15   8.55µs  10.07µs    95267.        0B     19.1
#>  3 best                 5          15   8.15µs   9.72µs    96795.        0B     29.0
#>  4 worst                5          15   8.54µs  10.04µs    94848.        0B     19.0
#>  5 best                10          15   8.15µs    9.7µs    97691.        0B     19.5
#>  6 worst               10          15   8.95µs  10.44µs    90222.        0B     27.1
#>  7 best                50          15   8.63µs  10.21µs    92905.        0B     18.6
#>  8 worst               50          15  12.16µs   13.6µs    70166.        0B     21.1
#>  9 best               100          15   9.09µs  10.67µs    89210.        0B     26.8
#> 10 worst              100          15  15.85µs   17.5µs    55322.        0B     11.1
#> 11 best                 3         100   8.35µs   9.89µs    96071.        0B     19.2
#> 12 worst                3         100   9.23µs  10.68µs    88434.        0B     26.5
#> 13 best                 5         100    8.2µs   9.76µs    97125.        0B     19.4
#> 14 worst                5         100    9.3µs  10.76µs    86236.        0B     25.9
#> 15 best                10         100   8.18µs    9.7µs    96109.        0B     28.8
#> 16 worst               10         100  10.58µs  12.11µs    79322.        0B     15.9
#> 17 best                50         100   8.62µs   9.98µs    96153.        0B     28.9
#> 18 worst               50         100  18.17µs  18.87µs    51919.        0B     10.4
#> 19 best               100         100   9.03µs   9.76µs    99324.        0B     19.9
#> 20 worst              100         100  28.37µs  29.18µs    33657.        0B     10.1
```
