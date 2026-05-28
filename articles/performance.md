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
#> 1 foo_S7(x)    7.26µs   8.88µs   105248.    10.8KB     21.1
#> 2 foo_S3(x)    2.47µs   2.89µs   315008.        0B      0  
#> 3 foo_S4(x)     2.7µs   3.27µs   293982.        0B     29.4

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
#> 1 bar_S7(x, y)   13.1µs  15.54µs    62612.        0B     18.8
#> 2 bar_S4(x, y)   6.89µs   8.16µs   118900.        0B     23.8
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
#>  1 best                 3          15    7.5µs   9.28µs   104462.        0B     20.9
#>  2 worst                3          15   7.56µs   9.34µs   103395.        0B     31.0
#>  3 best                 5          15   7.43µs    9.2µs   104940.        0B     31.5
#>  4 worst                5          15   7.72µs   9.48µs   101979.        0B     30.6
#>  5 best                10          15    7.6µs   9.31µs   104004.        0B     31.2
#>  6 worst               10          15    7.9µs   9.66µs   100457.        0B     20.1
#>  7 best                50          15   8.08µs   9.81µs    98613.        0B     29.6
#>  8 worst               50          15    9.6µs  11.33µs    85328.        0B     25.6
#>  9 best               100          15    8.7µs  10.57µs    91585.        0B     27.5
#> 10 worst              100          15  12.03µs  13.89µs    69999.        0B     21.0
#> 11 best                 3         100   7.61µs   9.45µs   100883.        0B     30.3
#> 12 worst                3         100   7.91µs   9.74µs    99259.        0B     29.8
#> 13 best                 5         100   7.63µs   9.43µs   102473.        0B     30.8
#> 14 worst                5         100   7.97µs   9.75µs    98206.        0B     29.5
#> 15 best                10         100   7.56µs   9.42µs   101144.        0B     30.4
#> 16 worst               10         100   8.34µs   10.1µs    95172.        0B     28.6
#> 17 best                50         100   8.11µs   9.98µs    95223.        0B     28.6
#> 18 worst               50         100  13.03µs  14.97µs    64373.        0B     19.3
#> 19 best               100         100    8.8µs  10.81µs    87859.        0B     26.4
#> 20 worst              100         100  19.62µs  21.44µs    45312.        0B     13.6
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
#>  1 best                 3          15   9.35µs   11.3µs    83694.        0B    33.5 
#>  2 worst                3          15   9.59µs   11.5µs    82985.        0B    24.9 
#>  3 best                 5          15   9.36µs   11.4µs    84106.        0B    25.2 
#>  4 worst                5          15   9.88µs   11.8µs    80352.        0B    32.2 
#>  5 best                10          15   9.44µs   11.4µs    83624.        0B    25.1 
#>  6 worst               10          15  10.04µs   11.2µs    85560.        0B    34.2 
#>  7 best                50          15  10.53µs   11.4µs    85527.        0B    34.2 
#>  8 worst               50          15  13.64µs   14.5µs    67587.        0B    20.3 
#>  9 best               100          15  11.76µs   12.7µs    76951.        0B    30.8 
#> 10 worst              100          15  18.35µs   19.6µs    49752.        0B    19.9 
#> 11 best                 3         100    9.5µs   10.9µs    88382.        0B    26.5 
#> 12 worst                3         100  10.34µs   11.7µs    82169.        0B    32.9 
#> 13 best                 5         100   9.35µs   10.8µs    89702.        0B    26.9 
#> 14 worst                5         100   10.5µs   11.8µs    81531.        0B    32.6 
#> 15 best                10         100   9.33µs   10.8µs    89046.        0B    26.7 
#> 16 worst               10         100  11.67µs   13.1µs    73894.        0B    29.6 
#> 17 best                50         100  10.71µs   12.1µs    79072.        0B    31.6 
#> 18 worst               50         100  20.19µs   21.6µs    45137.        0B    13.5 
#> 19 best               100         100  11.97µs   13.5µs    71549.        0B    35.8 
#> 20 worst              100         100  31.33µs     33µs    29565.        0B     8.87
```
