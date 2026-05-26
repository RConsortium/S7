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
#> 1 foo_S7(x)    7.28µs    8.5µs   110016.    10.8KB     22.0
#> 2 foo_S3(x)    2.48µs   2.82µs   323566.        0B      0  
#> 3 foo_S4(x)     2.7µs   3.13µs   307154.        0B     30.7

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
#> 1 bar_S7(x, y)  13.02µs  14.74µs    66031.        0B     19.8
#> 2 bar_S4(x, y)   6.98µs   7.85µs   124498.        0B     24.9
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
#>  1 best                 3          15   7.26µs   8.65µs   112247.        0B     22.5
#>  2 worst                3          15   7.61µs   8.88µs   109541.        0B     32.9
#>  3 best                 5          15   7.41µs    8.7µs   111566.        0B     33.5
#>  4 worst                5          15   7.63µs   8.91µs   109035.        0B     32.7
#>  5 best                10          15   7.57µs   8.83µs   110000.        0B     33.0
#>  6 worst               10          15   7.93µs   9.19µs   105842.        0B     21.2
#>  7 best                50          15   8.14µs   9.32µs   104200.        0B     31.3
#>  8 worst               50          15    9.7µs  10.97µs    88656.        0B     26.6
#>  9 best               100          15   8.68µs   9.97µs    97738.        0B     29.3
#> 10 worst              100          15  12.03µs   13.3µs    73397.        0B     22.0
#> 11 best                 3         100   7.55µs   8.74µs   110951.        0B     33.3
#> 12 worst                3         100   7.93µs   9.13µs   106321.        0B     31.9
#> 13 best                 5         100   7.56µs   8.79µs   110756.        0B     33.2
#> 14 worst                5         100   7.88µs   9.17µs   104343.        0B     31.3
#> 15 best                10         100   7.07µs    8.7µs   110492.        0B     33.2
#> 16 worst               10         100   8.16µs   9.47µs   102232.        0B     30.7
#> 17 best                50         100   8.02µs   9.33µs   103464.        0B     31.0
#> 18 worst               50         100  13.07µs  14.49µs    67087.        0B     20.1
#> 19 best               100         100   8.85µs  10.15µs    95092.        0B     28.5
#> 20 worst              100         100  19.45µs  20.91µs    46733.        0B     14.0
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
#>  1 best                 3          15   9.18µs   10.6µs    90160.        0B    36.1 
#>  2 worst                3          15   9.47µs   10.8µs    89532.        0B    26.9 
#>  3 best                 5          15   9.18µs   10.5µs    92080.        0B    27.6 
#>  4 worst                5          15   9.57µs   10.9µs    88004.        0B    35.2 
#>  5 best                10          15   9.48µs   10.8µs    89396.        0B    26.8 
#>  6 worst               10          15   9.82µs   10.7µs    89503.        0B    35.8 
#>  7 best                50          15  10.45µs     11µs    88677.        0B    35.5 
#>  8 worst               50          15  13.71µs   14.3µs    68683.        0B    20.6 
#>  9 best               100          15  11.53µs   12.2µs    80585.        0B    32.2 
#> 10 worst              100          15  18.21µs   19.1µs    51156.        0B    20.5 
#> 11 best                 3         100   9.42µs   10.3µs    94343.        0B    28.3 
#> 12 worst                3         100  10.21µs   11.1µs    87483.        0B    35.0 
#> 13 best                 5         100   9.35µs   10.3µs    92602.        0B    27.8 
#> 14 worst                5         100  10.41µs   11.3µs    85934.        0B    34.4 
#> 15 best                10         100   9.17µs   10.2µs    95215.        0B    28.6 
#> 16 worst               10         100   11.2µs   12.3µs    78672.        0B    31.5 
#> 17 best                50         100  10.72µs   11.7µs    83031.        0B    33.2 
#> 18 worst               50         100  20.06µs   21.2µs    46166.        0B    13.9 
#> 19 best               100         100  11.73µs   12.7µs    76033.        0B    38.0 
#> 20 worst              100         100  31.26µs   32.7µs    29892.        0B     8.97
```
