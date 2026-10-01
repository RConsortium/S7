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
#> 1 foo_S7(x)    6.36µs      7µs   135068.    10.9KB     27.0
#> 2 foo_S3(x)    2.56µs   2.83µs   319432.        0B     31.9
#> 3 foo_S4(x)    2.73µs   3.05µs   315854.        0B     31.6

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
#> 1 bar_S7(x, y)     13µs  14.17µs    67591.        0B     20.3
#> 2 bar_S4(x, y)   7.12µs   8.02µs   120889.        0B     24.2
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
#>  1 best                 3          15   6.38µs   7.12µs   135714.        0B    13.6 
#>  2 worst                3          15   6.62µs   7.29µs   132372.        0B    26.5 
#>  3 best                 5          15   6.34µs   7.09µs   135285.        0B    27.1 
#>  4 worst                5          15    6.7µs   7.38µs   130227.        0B    26.1 
#>  5 best                10          15    6.4µs   7.09µs   134591.        0B    26.9 
#>  6 worst               10          15    6.9µs    7.6µs   126505.        0B    25.3 
#>  7 best                50          15   6.58µs    7.3µs   131882.        0B    26.4 
#>  8 worst               50          15   8.58µs    9.3µs   104219.        0B    20.8 
#>  9 best               100          15   6.83µs   7.54µs   128084.        0B    25.6 
#> 10 worst              100          15   10.8µs  11.53µs    84020.        0B    16.8 
#> 11 best                 3         100   6.51µs   7.27µs   132833.        0B    26.6 
#> 12 worst                3         100   6.91µs   7.67µs   125520.        0B    25.1 
#> 13 best                 5         100   6.58µs   7.34µs   131061.        0B    13.1 
#> 14 worst                5         100   7.04µs   7.76µs   123533.        0B    24.7 
#> 15 best                10         100   6.52µs   7.23µs   132614.        0B    26.5 
#> 16 worst               10         100   7.35µs   8.05µs   119817.        0B    24.0 
#> 17 best                50         100   6.55µs   7.32µs   131675.        0B    13.2 
#> 18 worst               50         100  12.13µs  12.88µs    75172.        0B    15.0 
#> 19 best               100         100   6.99µs   7.76µs   124343.        0B    12.4 
#> 20 worst              100         100  18.66µs  19.52µs    49723.        0B     9.95
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
#>  1 best                 3          15    8.8µs   9.85µs    97400.        0B    29.2 
#>  2 worst                3          15   9.29µs  10.36µs    92441.        0B    18.5 
#>  3 best                 5          15   8.74µs   9.82µs    95380.        0B    28.6 
#>  4 worst                5          15   9.42µs  10.45µs    91128.        0B    27.3 
#>  5 best                10          15      9µs  10.01µs    95598.        0B    19.1 
#>  6 worst               10          15   9.76µs  10.81µs    87889.        0B    26.4 
#>  7 best                50          15   9.41µs  10.43µs    91854.        0B    18.4 
#>  8 worst               50          15  13.35µs  14.35µs    66622.        0B    20.0 
#>  9 best               100          15   9.89µs  10.98µs    85923.        0B    17.2 
#> 10 worst              100          15  17.82µs  19.02µs    50535.        0B    15.2 
#> 11 best                 3         100   9.07µs  10.07µs    92961.        0B    18.6 
#> 12 worst                3         100   9.91µs  10.96µs    86303.        0B    25.9 
#> 13 best                 5         100   8.95µs  10.01µs    94822.        0B    28.5 
#> 14 worst                5         100  10.07µs  11.19µs    85738.        0B    17.2 
#> 15 best                10         100   8.91µs  10.06µs    95095.        0B    19.0 
#> 16 worst               10         100  11.37µs  12.46µs    76737.        0B    23.0 
#> 17 best                50         100   9.31µs   9.91µs    97606.        0B    19.5 
#> 18 worst               50         100  19.34µs   20.2µs    44174.        0B    13.3 
#> 19 best               100         100   9.74µs  10.34µs    93713.        0B    28.1 
#> 20 worst              100         100  30.11µs  30.93µs    31563.        0B     9.47
```
