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
#> 1 foo_S7(x)    7.64µs   9.46µs   100042.    10.8KB     30.0
#> 2 foo_S3(x)    2.48µs   2.87µs   316764.        0B     31.7
#> 3 foo_S4(x)    2.67µs   3.21µs   299731.        0B     30.0

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
#> 1 bar_S7(x, y)  13.71µs  16.19µs    60054.        0B     30.0
#> 2 bar_S4(x, y)   7.01µs   8.23µs   118053.        0B     23.6
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
#>  1 best                 3          15   7.76µs   9.64µs    98624.        0B     29.6
#>  2 worst                3          15   8.01µs   9.95µs    97368.        0B     29.2
#>  3 best                 5          15   7.89µs   9.94µs    79567.        0B     23.9
#>  4 worst                5          15   8.26µs  10.36µs    93441.        0B     28.0
#>  5 best                10          15   7.95µs   9.85µs    98537.        0B     29.6
#>  6 worst               10          15   8.36µs  10.26µs    94551.        0B     28.4
#>  7 best                50          15   7.99µs  10.05µs    95959.        0B     28.8
#>  8 worst               50          15   9.84µs  11.75µs    82293.        0B     24.7
#>  9 best               100          15   8.24µs  10.31µs    93613.        0B     28.1
#> 10 worst              100          15  11.63µs  13.59µs    71262.        0B     28.5
#> 11 best                 3         100   8.03µs  10.04µs    96164.        0B     28.9
#> 12 worst                3         100    8.4µs  10.31µs    93240.        0B     37.3
#> 13 best                 5         100   8.01µs  10.15µs    94444.        0B     28.3
#> 14 worst                5         100   8.51µs  10.45µs    91151.        0B     27.4
#> 15 best                10         100   8.13µs   10.1µs    94647.        0B     28.4
#> 16 worst               10         100   8.73µs  10.85µs    87995.        0B     35.2
#> 17 best                50         100   8.03µs   9.99µs    95633.        0B     28.7
#> 18 worst               50         100  13.19µs  15.33µs    62739.        0B     25.1
#> 19 best               100         100   8.53µs  10.64µs    88371.        0B     26.5
#> 20 worst              100         100  19.24µs  21.46µs    45209.        0B     13.6
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
#>  1 best                 3          15   10.1µs   12.4µs    75607.        0B     30.3
#>  2 worst                3          15   10.6µs   12.8µs    74431.        0B     29.8
#>  3 best                 5          15   10.3µs   12.6µs    73120.        0B     36.6
#>  4 worst                5          15   10.2µs   11.9µs    79236.        0B     23.8
#>  5 best                10          15   10.1µs   11.1µs    87528.        0B     35.0
#>  6 worst               10          15   10.9µs   11.9µs    81516.        0B     32.6
#>  7 best                50          15   10.5µs   11.5µs    84832.        0B     33.9
#>  8 worst               50          15   13.8µs   14.8µs    65860.        0B     26.4
#>  9 best               100          15   11.1µs   12.6µs    75031.        0B     30.0
#> 10 worst              100          15   17.6µs   19.2µs    50031.        0B     20.0
#> 11 best                 3         100   10.3µs   11.9µs    79793.        0B     31.9
#> 12 worst                3         100   10.9µs   12.6µs    74111.        0B     29.7
#> 13 best                 5         100   10.1µs   11.7µs    81322.        0B     32.5
#> 14 worst                5         100   11.2µs   12.7µs    74792.        0B     29.9
#> 15 best                10         100   10.2µs   11.8µs    79659.        0B     31.9
#> 16 worst               10         100   12.4µs   14.1µs    67488.        0B     27.0
#> 17 best                50         100   10.8µs   12.4µs    76731.        0B     30.7
#> 18 worst               50         100   20.2µs   21.8µs    44220.        0B     17.7
#> 19 best               100         100   11.2µs   12.8µs    73371.        0B     29.4
#> 20 worst              100         100   30.4µs   32.2µs    30075.        0B     12.0
```
