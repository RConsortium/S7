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
#> 1 foo_S7(x)    6.38µs   7.23µs   130511.    10.9KB     26.1
#> 2 foo_S3(x)    2.56µs   2.87µs   316000.        0B     31.6
#> 3 foo_S4(x)    2.75µs   3.11µs   309591.        0B     31.0

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
#> 1 bar_S7(x, y)   13.4µs  14.65µs    65676.        0B     19.7
#> 2 bar_S4(x, y)   7.39µs   8.29µs   116788.        0B     11.7
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
#>  1 best                 3          15   6.19µs   7.22µs   133921.        0B    26.8 
#>  2 worst                3          15   6.55µs   7.37µs   131154.        0B    26.2 
#>  3 best                 5          15   6.39µs   7.29µs   132456.        0B    26.5 
#>  4 worst                5          15   6.63µs   7.54µs   128108.        0B    25.6 
#>  5 best                10          15   6.44µs   7.26µs   132177.        0B    26.4 
#>  6 worst               10          15   6.91µs   7.75µs   125094.        0B    12.5 
#>  7 best                50          15   6.67µs   7.54µs   127829.        0B    25.6 
#>  8 worst               50          15   8.58µs   9.52µs   101638.        0B    20.3 
#>  9 best               100          15   6.85µs   7.72µs   124388.        0B    24.9 
#> 10 worst              100          15     11µs     12µs    80852.        0B    16.2 
#> 11 best                 3         100   6.61µs   7.48µs   128873.        0B    25.8 
#> 12 worst                3         100   6.92µs   7.89µs   121791.        0B    24.4 
#> 13 best                 5         100   6.52µs   7.43µs   128457.        0B    25.7 
#> 14 worst                5         100   6.99µs   7.86µs   122017.        0B    24.4 
#> 15 best                10         100    6.5µs   7.47µs   128313.        0B    25.7 
#> 16 worst               10         100   7.33µs   8.21µs   117919.        0B    11.8 
#> 17 best                50         100   6.77µs   7.62µs   125375.        0B    25.1 
#> 18 worst               50         100  12.29µs  13.23µs    72649.        0B    14.5 
#> 19 best               100         100   7.05µs   7.89µs   121052.        0B    24.2 
#> 20 worst              100         100  18.66µs  19.62µs    49434.        0B     9.89
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
#>  1 best                 3          15   8.88µs   10.1µs    94145.        0B    28.3 
#>  2 worst                3          15   9.19µs   10.5µs    91021.        0B    18.2 
#>  3 best                 5          15   8.88µs     10µs    94845.        0B    19.0 
#>  4 worst                5          15   9.44µs   10.5µs    90199.        0B    27.1 
#>  5 best                10          15   9.06µs   10.2µs    93258.        0B    28.0 
#>  6 worst               10          15   9.94µs   11.1µs    85714.        0B    17.1 
#>  7 best                50          15   9.29µs   10.4µs    92074.        0B    27.6 
#>  8 worst               50          15  13.14µs   14.3µs    67761.        0B    13.6 
#>  9 best               100          15   9.89µs     11µs    86223.        0B    17.2 
#> 10 worst              100          15  17.78µs   18.9µs    50767.        0B    15.2 
#> 11 best                 3         100   9.11µs   10.3µs    91946.        0B    27.6 
#> 12 worst                3         100   9.95µs   11.1µs    86287.        0B    17.3 
#> 13 best                 5         100    9.1µs   10.2µs    93441.        0B    18.7 
#> 14 worst                5         100  10.22µs   11.3µs    84147.        0B    25.3 
#> 15 best                10         100   8.68µs    9.7µs    98968.        0B    29.7 
#> 16 worst               10         100  11.11µs   11.8µs    82885.        0B    16.6 
#> 17 best                50         100   9.43µs   10.1µs    95849.        0B    28.8 
#> 18 worst               50         100  19.56µs   20.2µs    48153.        0B     9.63
#> 19 best               100         100    9.9µs   10.6µs    91282.        0B    27.4 
#> 20 worst              100         100  30.46µs   31.4µs    31036.        0B     6.21
```
