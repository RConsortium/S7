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
#> 1 foo_S7(x)    6.47µs   7.39µs   126775.    10.9KB     25.4
#> 2 foo_S3(x)    2.62µs   2.96µs   307470.        0B     30.8
#> 3 foo_S4(x)    2.73µs   3.21µs   269752.        0B     27.0

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
#> 1 bar_S7(x, y)   13.4µs  15.03µs    63745.        0B     25.5
#> 2 bar_S4(x, y)   7.54µs   8.91µs   102379.        0B     10.2
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
#>  1 best                 3          15   6.38µs   7.44µs   129413.        0B    25.9 
#>  2 worst                3          15   6.67µs   7.57µs   127556.        0B    25.5 
#>  3 best                 5          15   6.48µs   7.41µs   129855.        0B    26.0 
#>  4 worst                5          15   6.77µs   7.71µs   125359.        0B    25.1 
#>  5 best                10          15   6.42µs   7.38µs   130221.        0B    26.0 
#>  6 worst               10          15   6.99µs   8.02µs   118417.        0B    11.8 
#>  7 best                50          15   6.74µs   7.65µs   125791.        0B    25.2 
#>  8 worst               50          15   8.69µs   9.68µs    99816.        0B    20.0 
#>  9 best               100          15   6.95µs   7.97µs   119223.        0B    23.8 
#> 10 worst              100          15  10.97µs  12.05µs    80139.        0B    16.0 
#> 11 best                 3         100   6.51µs   7.44µs   125533.        0B    25.1 
#> 12 worst                3         100   6.92µs   7.95µs   120329.        0B    24.1 
#> 13 best                 5         100   6.65µs   7.56µs   125792.        0B    25.2 
#> 14 worst                5         100   7.06µs   8.04µs   118979.        0B    23.8 
#> 15 best                10         100    6.6µs   7.62µs   125230.        0B    25.1 
#> 16 worst               10         100   7.37µs    8.5µs   113317.        0B    11.3 
#> 17 best                50         100   6.64µs   7.68µs   123130.        0B    24.6 
#> 18 worst               50         100  12.13µs  13.18µs    73349.        0B    14.7 
#> 19 best               100         100      7µs   8.21µs   112598.        0B    22.5 
#> 20 worst              100         100  18.71µs  19.82µs    48914.        0B     9.78
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
#>  1 best                 3          15   8.84µs   10.3µs    91680.        0B    27.5 
#>  2 worst                3          15   9.36µs   10.8µs    87063.        0B    17.4 
#>  3 best                 5          15   9.01µs   10.2µs    92730.        0B    27.8 
#>  4 worst                5          15   9.56µs   10.8µs    88066.        0B    26.4 
#>  5 best                10          15   8.97µs   10.2µs    93474.        0B    28.1 
#>  6 worst               10          15   9.92µs   11.2µs    85090.        0B    17.0 
#>  7 best                50          15    9.4µs   11.1µs    82511.        0B    24.8 
#>  8 worst               50          15  13.39µs   14.6µs    65974.        0B    13.2 
#>  9 best               100          15   9.89µs   11.2µs    84449.        0B    25.3 
#> 10 worst              100          15  18.02µs   19.3µs    49806.        0B     9.96
#> 11 best                 3         100    9.3µs   10.6µs    89967.        0B    18.0 
#> 12 worst                3         100  10.18µs   11.4µs    83200.        0B    25.0 
#> 13 best                 5         100   9.06µs   10.3µs    90565.        0B    27.2 
#> 14 worst                5         100  10.21µs   11.5µs    82340.        0B    24.7 
#> 15 best                10         100   8.67µs     10µs    96315.        0B    19.3 
#> 16 worst               10         100  11.06µs   11.8µs    82312.        0B    24.7 
#> 17 best                50         100   9.53µs   10.3µs    94311.        0B    18.9 
#> 18 worst               50         100  19.48µs   20.4µs    46476.        0B    13.9 
#> 19 best               100         100   9.86µs   10.6µs    90190.        0B    18.0 
#> 20 worst              100         100  30.52µs   31.5µs    30692.        0B     9.21
```
