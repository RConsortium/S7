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
#> 1 foo_S7(x)    7.33µs   8.54µs   109192.    18.2KB     21.8
#> 2 foo_S3(x)    2.58µs   2.83µs   322219.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.16µs   304602.        0B     30.5

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
#> 1 bar_S7(x, y)   13.2µs  14.88µs    64869.        0B     26.0
#> 2 bar_S4(x, y)    7.4µs   8.38µs   115798.        0B     23.2
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
#>  1 best                 3          15   7.18µs   8.47µs   114294.        0B     22.9
#>  2 worst                3          15    7.4µs   8.75µs   110219.        0B     33.1
#>  3 best                 5          15    7.2µs   8.57µs   112657.        0B     33.8
#>  4 worst                5          15   7.55µs   8.82µs   108765.        0B     32.6
#>  5 best                10          15   7.28µs   8.59µs   112388.        0B     22.5
#>  6 worst               10          15   7.72µs   9.09µs   106154.        0B     31.9
#>  7 best                50          15   7.75µs   9.12µs   105766.        0B     31.7
#>  8 worst               50          15   9.48µs  10.85µs    88377.        0B     26.5
#>  9 best               100          15   8.29µs   9.72µs    98981.        0B     29.7
#> 10 worst              100          15  11.97µs   13.5µs    71568.        0B     21.5
#> 11 best                 3         100   7.25µs   8.72µs   109009.        0B     32.7
#> 12 worst                3         100   7.59µs   9.04µs   106318.        0B     31.9
#> 13 best                 5         100   7.24µs   8.75µs   109557.        0B     32.9
#> 14 worst                5         100   7.71µs   9.14µs   104663.        0B     31.4
#> 15 best                10         100   7.41µs   8.84µs   106608.        0B     32.0
#> 16 worst               10         100   8.22µs   9.73µs    97361.        0B     29.2
#> 17 best                50         100   7.89µs   9.35µs   101889.        0B     20.4
#> 18 worst               50         100  13.15µs  14.71µs    65101.        0B     19.5
#> 19 best               100         100   8.56µs   9.97µs    95572.        0B     28.7
#> 20 worst              100         100  19.77µs   21.4µs    44608.        0B     13.4
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
#>  1 best                 3          15   8.89µs   10.7µs    88570.        0B     35.4
#>  2 worst                3          15   9.43µs   11.3µs    83841.        0B     25.2
#>  3 best                 5          15   9.23µs     11µs    85970.        0B     34.4
#>  4 worst                5          15   9.66µs   11.4µs    82258.        0B     24.7
#>  5 best                10          15   9.31µs     11µs    86054.        0B     34.4
#>  6 worst               10          15   9.58µs   11.5µs    82864.        0B     24.9
#>  7 best                50          15    9.9µs   10.8µs    89073.        0B     35.6
#>  8 worst               50          15  13.35µs   14.2µs    68159.        0B     20.5
#>  9 best               100          15  11.14µs   12.1µs    79929.        0B     32.0
#> 10 worst              100          15  18.04µs   19.1µs    50603.        0B     20.2
#> 11 best                 3         100   9.33µs   10.5µs    90904.        0B     27.3
#> 12 worst                3         100  10.13µs   11.4µs    84026.        0B     33.6
#> 13 best                 5         100   9.01µs   10.3µs    93515.        0B     28.1
#> 14 worst                5         100  10.12µs   11.4µs    83929.        0B     33.6
#> 15 best                10         100   9.21µs   10.4µs    92104.        0B     27.6
#> 16 worst               10         100  11.39µs   12.7µs    75994.        0B     30.4
#> 17 best                50         100  10.39µs   11.7µs    81747.        0B     24.5
#> 18 worst               50         100  19.94µs   21.4µs    44980.        0B     18.0
#> 19 best               100         100  11.36µs   12.6µs    76678.        0B     30.7
#> 20 worst              100         100  31.29µs   32.7µs    29650.        0B     11.9
```
