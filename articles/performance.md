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
#> 1 foo_S7(x)    7.32µs   8.72µs   107308.    10.8KB     21.5
#> 2 foo_S3(x)    2.47µs   2.87µs   318244.        0B      0  
#> 3 foo_S4(x)    2.68µs   3.21µs   298096.        0B     29.8

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
#> 1 bar_S7(x, y)  12.79µs  14.83µs    65930.        0B     26.4
#> 2 bar_S4(x, y)   6.97µs   8.01µs   121010.        0B     24.2
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
#>  1 best                 3          15   7.41µs   9.19µs   104928.        0B     21.0
#>  2 worst                3          15   7.58µs   9.33µs   103407.        0B     31.0
#>  3 best                 5          15   7.49µs    9.2µs   104949.        0B     31.5
#>  4 worst                5          15   7.66µs   9.46µs   102024.        0B     30.6
#>  5 best                10          15   7.45µs   9.25µs   104080.        0B     31.2
#>  6 worst               10          15   7.85µs   9.63µs    99317.        0B     19.9
#>  7 best                50          15   7.99µs    9.8µs    98575.        0B     29.6
#>  8 worst               50          15   9.48µs  11.33µs    85654.        0B     25.7
#>  9 best               100          15   8.59µs   10.4µs    93012.        0B     27.9
#> 10 worst              100          15  11.93µs  13.71µs    70959.        0B     21.3
#> 11 best                 3         100   7.33µs   9.23µs   104283.        0B     31.3
#> 12 worst                3         100   7.87µs   9.56µs   100891.        0B     30.3
#> 13 best                 5         100   7.49µs    9.2µs   104092.        0B     41.7
#> 14 worst                5         100   7.77µs   9.56µs   101304.        0B     30.4
#> 15 best                10         100   7.47µs    9.3µs   102564.        0B     20.5
#> 16 worst               10         100   8.17µs  10.01µs    95255.        0B     28.6
#> 17 best                50         100   7.93µs   9.65µs    99653.        0B     29.9
#> 18 worst               50         100  13.08µs   14.9µs    64746.        0B     19.4
#> 19 best               100         100   8.74µs  10.35µs    93018.        0B     27.9
#> 20 worst              100         100  19.26µs  21.07µs    46119.        0B     13.8
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
#>  1 best                 3          15   9.15µs   11.2µs    85181.        0B     34.1
#>  2 worst                3          15   9.53µs   11.5µs    83438.        0B     25.0
#>  3 best                 5          15   9.14µs   11.2µs    85210.        0B     34.1
#>  4 worst                5          15   9.55µs   11.8µs    80816.        0B     24.3
#>  5 best                10          15   9.24µs   11.6µs    82090.        0B     32.8
#>  6 worst               10          15   9.79µs     11µs    86309.        0B     25.9
#>  7 best                50          15  10.29µs     11µs    88666.        0B     26.6
#>  8 worst               50          15  13.59µs   14.5µs    67647.        0B     27.1
#>  9 best               100          15  11.67µs   12.4µs    78343.        0B     39.2
#> 10 worst              100          15  18.35µs   19.5µs    49747.        0B     14.9
#> 11 best                 3         100   9.34µs   10.8µs    88292.        0B     26.5
#> 12 worst                3         100  10.26µs   11.8µs    81799.        0B     24.5
#> 13 best                 5         100    9.3µs   10.8µs    87820.        0B     26.4
#> 14 worst                5         100  10.21µs   11.8µs    81379.        0B     24.4
#> 15 best                10         100   9.36µs   10.8µs    88804.        0B     26.6
#> 16 worst               10         100  11.73µs   13.3µs    72559.        0B     29.0
#> 17 best                50         100  10.68µs   12.2µs    78582.        0B     23.6
#> 18 worst               50         100  20.03µs   21.7µs    44697.        0B     17.9
#> 19 best               100         100  11.87µs   13.1µs    73914.        0B     29.6
#> 20 worst              100         100  31.17µs   32.8µs    29750.        0B     11.9
```
