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
#> 1 foo_S7(x)    6.31µs    7.7µs   123345.    10.8KB     37.0
#> 2 foo_S3(x)    1.94µs   2.27µs   403660.        0B      0  
#> 3 foo_S4(x)    2.06µs   2.54µs   377255.        0B     37.7

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
#> 1 bar_S7(x, y)   11.3µs  13.38µs    72841.        0B     36.4
#> 2 bar_S4(x, y)    5.4µs   6.37µs   152964.        0B     15.3
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
#>  1 best                 3          15   6.25µs    7.7µs   126069.        0B     37.8
#>  2 worst                3          15   6.39µs   7.87µs   123401.        0B     49.4
#>  3 best                 5          15   6.35µs   7.57µs   128493.        0B     38.6
#>  4 worst                5          15   6.58µs   7.78µs   124712.        0B     49.9
#>  5 best                10          15   6.52µs   7.97µs   121973.        0B     36.6
#>  6 worst               10          15   6.83µs   8.32µs   116080.        0B     46.5
#>  7 best                50          15   6.85µs   8.46µs   114623.        0B     34.4
#>  8 worst               50          15   8.14µs   9.68µs   100380.        0B     30.1
#>  9 best               100          15   7.19µs   8.73µs   111443.        0B     33.4
#> 10 worst              100          15   9.74µs  11.14µs    87629.        0B     35.1
#> 11 best                 3         100   6.33µs   7.61µs   127720.        0B     38.3
#> 12 worst                3         100   6.68µs   7.95µs   121209.        0B     48.5
#> 13 best                 5         100   6.43µs   7.66µs   126136.        0B     37.9
#> 14 worst                5         100   6.76µs   7.96µs   120953.        0B     48.4
#> 15 best                10         100   6.58µs    7.8µs   123769.        0B     37.1
#> 16 worst               10         100   7.07µs   8.41µs   114233.        0B     45.7
#> 17 best                50         100   6.86µs   8.53µs   112165.        0B     33.7
#> 18 worst               50         100  10.72µs   12.5µs    77794.        0B     23.3
#> 19 best               100         100    7.5µs   9.21µs   104605.        0B     31.4
#> 20 worst              100         100  15.84µs  17.64µs    55036.        0B     22.0
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
#>  1 best                 3          15   8.46µs  10.27µs    94347.        0B     37.8
#>  2 worst                3          15   8.26µs  10.34µs    93560.        0B     46.8
#>  3 best                 5          15   8.23µs   9.01µs   107861.        0B     54.0
#>  4 worst                5          15   8.55µs   9.43µs   103455.        0B     41.4
#>  5 best                10          15   8.27µs   9.05µs   106963.        0B     53.5
#>  6 worst               10          15   8.85µs   9.63µs   101541.        0B     40.6
#>  7 best                50          15   9.22µs  10.29µs    93909.        0B     37.6
#>  8 worst               50          15  11.57µs  12.96µs    75114.        0B     30.1
#>  9 best               100          15  10.26µs   11.6µs    83833.        0B     33.5
#> 10 worst              100          15  15.39µs  16.68µs    58707.        0B     23.5
#> 11 best                 3         100   8.55µs   9.73µs    99242.        0B     49.6
#> 12 worst                3         100   9.31µs   10.3µs    93518.        0B     37.4
#> 13 best                 5         100   8.59µs   9.66µs    99179.        0B     49.6
#> 14 worst                5         100   9.08µs  10.21µs    94570.        0B     37.8
#> 15 best                10         100   8.53µs   9.57µs   100735.        0B     40.3
#> 16 worst               10         100  10.13µs  11.11µs    87239.        0B     43.6
#> 17 best                50         100   9.51µs  10.57µs    91730.        0B     36.7
#> 18 worst               50         100  16.75µs  17.96µs    54089.        0B     27.1
#> 19 best               100         100  10.42µs  11.45µs    84509.        0B     42.3
#> 20 worst              100         100  25.54µs  26.88µs    36114.        0B     14.5
```
