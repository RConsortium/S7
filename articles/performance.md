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
#> 1 foo_S7(x)    7.26µs   9.01µs   103157.    10.8KB     20.6
#> 2 foo_S3(x)    2.58µs   2.96µs   304725.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.35µs   284140.        0B     28.4

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
#> 1 bar_S7(x, y)     13µs  15.43µs    63078.        0B     25.2
#> 2 bar_S4(x, y)    6.9µs   8.27µs   116705.        0B     23.3
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
#>  1 best                 3          15   7.53µs   9.37µs   102345.        0B     20.5
#>  2 worst                3          15   7.63µs   9.63µs    99843.        0B     30.0
#>  3 best                 5          15   7.46µs   9.38µs   102340.        0B     30.7
#>  4 worst                5          15   7.67µs    9.6µs   100243.        0B     30.1
#>  5 best                10          15   7.61µs   9.54µs   100176.        0B     30.1
#>  6 worst               10          15   7.85µs   9.77µs    99120.        0B     19.8
#>  7 best                50          15   8.14µs  10.04µs    95817.        0B     28.8
#>  8 worst               50          15   9.66µs  11.62µs    83085.        0B     24.9
#>  9 best               100          15   8.71µs  10.81µs    88927.        0B     26.7
#> 10 worst              100          15  12.01µs  14.05µs    68980.        0B     20.7
#> 11 best                 3         100   7.49µs   9.43µs   101720.        0B     30.5
#> 12 worst                3         100   7.78µs   9.82µs    98152.        0B     29.5
#> 13 best                 5         100   7.67µs   9.57µs   100634.        0B     30.2
#> 14 worst                5         100   7.97µs   9.96µs    96103.        0B     28.8
#> 15 best                10         100   7.53µs   9.61µs    97728.        0B     19.5
#> 16 worst               10         100   8.24µs  10.36µs    91008.        0B     27.3
#> 17 best                50         100   8.02µs  10.18µs    93664.        0B     28.1
#> 18 worst               50         100  13.06µs  15.21µs    62126.        0B     12.4
#> 19 best               100         100    8.8µs  10.96µs    86655.        0B     26.0
#> 20 worst              100         100  19.45µs  21.71µs    44232.        0B     13.3
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
#>  1 best                 3          15   9.33µs   11.7µs    80446.        0B     24.1
#>  2 worst                3          15   9.48µs   12.4µs    73607.        0B     29.5
#>  3 best                 5          15   9.49µs   12.9µs    60608.        0B     18.2
#>  4 worst                5          15   9.85µs   13.2µs    63610.        0B     25.5
#>  5 best                10          15   9.59µs     12µs    78714.        0B     23.6
#>  6 worst               10          15   9.95µs   11.8µs    78992.        0B     31.6
#>  7 best                50          15  10.31µs   11.4µs    84178.        0B     33.7
#>  8 worst               50          15  13.53µs   14.6µs    66108.        0B     19.8
#>  9 best               100          15  11.63µs   12.7µs    75904.        0B     30.4
#> 10 worst              100          15  18.05µs   19.3µs    50166.        0B     20.1
#> 11 best                 3         100    9.5µs   11.2µs    81012.        0B     24.3
#> 12 worst                3         100   9.97µs   11.7µs    79767.        0B     31.9
#> 13 best                 5         100    9.2µs   10.8µs    88264.        0B     26.5
#> 14 worst                5         100  10.52µs   12.1µs    79108.        0B     31.7
#> 15 best                10         100   9.64µs   11.1µs    86326.        0B     25.9
#> 16 worst               10         100  11.45µs   13.3µs    72120.        0B     28.9
#> 17 best                50         100  10.79µs   12.4µs    76624.        0B     30.7
#> 18 worst               50         100  20.19µs   21.7µs    44807.        0B     13.4
#> 19 best               100         100     12µs   13.5µs    70752.        0B     28.3
#> 20 worst              100         100  31.26µs   33.2µs    29279.        0B     11.7
```
