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
#> 1 foo_S7(x)    7.69µs    8.9µs   104955.    10.8KB     21.0
#> 2 foo_S3(x)    2.54µs   2.79µs   321452.        0B      0  
#> 3 foo_S4(x)    2.73µs   3.11µs   308135.        0B     30.8

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
#> 1 bar_S7(x, y)   13.9µs  15.75µs    60679.        0B     18.2
#> 2 bar_S4(x, y)   7.24µs   8.22µs   115224.        0B     23.0
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
#>  1 best                 3          15   7.57µs   8.97µs   107375.        0B     21.5
#>  2 worst                3          15   7.86µs   9.23µs   104454.        0B     31.3
#>  3 best                 5          15   7.57µs   8.94µs   107616.        0B     32.3
#>  4 worst                5          15   7.88µs   9.24µs   104539.        0B     31.4
#>  5 best                10          15   7.74µs   9.14µs   105902.        0B     31.8
#>  6 worst               10          15   8.21µs   9.57µs   100872.        0B     20.2
#>  7 best                50          15   8.23µs    9.5µs   101847.        0B     30.6
#>  8 worst               50          15   10.1µs  11.48µs    84418.        0B     25.3
#>  9 best               100          15   8.86µs  10.27µs    93732.        0B     28.1
#> 10 worst              100          15  12.89µs  14.14µs    68647.        0B     27.5
#> 11 best                 3         100   7.74µs   8.97µs   107611.        0B     32.3
#> 12 worst                3         100   8.05µs   9.63µs   100222.        0B     30.1
#> 13 best                 5         100   7.92µs   9.26µs   104348.        0B     31.3
#> 14 worst                5         100   8.12µs   9.64µs    99208.        0B     29.8
#> 15 best                10         100   7.93µs   9.36µs   100537.        0B     30.2
#> 16 worst               10         100    8.6µs  10.18µs    91942.        0B     27.6
#> 17 best                50         100   8.22µs   9.84µs    96719.        0B     29.0
#> 18 worst               50         100  13.75µs  15.34µs    62529.        0B     18.8
#> 19 best               100         100   9.04µs  10.66µs    89730.        0B     26.9
#> 20 worst              100         100  20.48µs  22.16µs    43460.        0B     13.0
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
#>  1 best                 3          15   9.65µs   11.3µs    84248.        0B    33.7 
#>  2 worst                3          15   9.91µs   11.4µs    83402.        0B    25.0 
#>  3 best                 5          15   9.57µs   11.2µs    85205.        0B    34.1 
#>  4 worst                5          15  10.02µs   11.6µs    79880.        0B    24.0 
#>  5 best                10          15   9.75µs   11.5µs    82671.        0B    33.1 
#>  6 worst               10          15  10.24µs   11.4µs    84146.        0B    25.3 
#>  7 best                50          15  10.44µs   11.3µs    86149.        0B    25.9 
#>  8 worst               50          15  14.35µs   15.2µs    64145.        0B    25.7 
#>  9 best               100          15  11.69µs   12.4µs    77943.        0B    31.2 
#> 10 worst              100          15  19.34µs   20.6µs    46333.        0B    18.5 
#> 11 best                 3         100   9.82µs   11.2µs    84683.        0B    25.4 
#> 12 worst                3         100  10.47µs   11.9µs    78367.        0B    31.4 
#> 13 best                 5         100   9.36µs   10.9µs    86928.        0B    34.8 
#> 14 worst                5         100  10.57µs   11.9µs    79995.        0B    24.0 
#> 15 best                10         100    9.7µs   11.2µs    86011.        0B    34.4 
#> 16 worst               10         100  12.02µs   13.3µs    72720.        0B    21.8 
#> 17 best                50         100     11µs   12.3µs    77796.        0B    31.1 
#> 18 worst               50         100  20.87µs   22.5µs    42962.        0B    17.2 
#> 19 best               100         100  12.04µs   13.4µs    70979.        0B    35.5 
#> 20 worst              100         100  32.47µs   34.2µs    28240.        0B     8.47
```
