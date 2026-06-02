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
#> 1 foo_S7(x)    7.53µs   8.67µs   107863.    10.8KB     21.6
#> 2 foo_S3(x)     2.5µs   2.71µs   334000.        0B      0  
#> 3 foo_S4(x)    2.69µs   2.99µs   321162.        0B     32.1

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
#> 1 bar_S7(x, y)  13.54µs  15.12µs    63889.        0B     25.6
#> 2 bar_S4(x, y)   7.08µs   7.92µs   122544.        0B     24.5
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
#>  1 best                 3          15   7.51µs   8.87µs   109663.        0B     21.9
#>  2 worst                3          15   7.78µs      9µs   107533.        0B     32.3
#>  3 best                 5          15   7.57µs   8.65µs   112171.        0B     33.7
#>  4 worst                5          15   7.88µs    8.9µs   109015.        0B     32.7
#>  5 best                10          15   7.62µs   8.88µs   109460.        0B     21.9
#>  6 worst               10          15   8.04µs   9.24µs   105030.        0B     31.5
#>  7 best                50          15   8.12µs   9.27µs   105030.        0B     31.5
#>  8 worst               50          15  10.06µs  11.24µs    86404.        0B     25.9
#>  9 best               100          15   8.83µs  10.06µs    96089.        0B     28.8
#> 10 worst              100          15  12.75µs  13.94µs    69793.        0B     20.9
#> 11 best                 3         100   7.67µs   8.86µs   108913.        0B     32.7
#> 12 worst                3         100   8.11µs    9.2µs   104759.        0B     31.4
#> 13 best                 5         100   7.71µs   8.94µs   108466.        0B     32.5
#> 14 worst                5         100   8.15µs   9.44µs   102374.        0B     30.7
#> 15 best                10         100   7.79µs   8.98µs   107167.        0B     21.4
#> 16 worst               10         100   8.71µs  10.02µs    96028.        0B     28.8
#> 17 best                50         100   8.13µs   9.36µs   103368.        0B     31.0
#> 18 worst               50         100  13.57µs  14.93µs    64838.        0B     13.0
#> 19 best               100         100   8.92µs  10.18µs    94931.        0B     28.5
#> 20 worst              100         100  20.53µs  21.97µs    44207.        0B     13.3
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
#>  1 best                 3          15   9.32µs     11µs    87560.        0B     26.3
#>  2 worst                3          15   9.61µs   10.9µs    87883.        0B     35.2
#>  3 best                 5          15   9.29µs   10.7µs    90217.        0B     27.1
#>  4 worst                5          15    9.8µs   11.2µs    86573.        0B     34.6
#>  5 best                10          15   9.53µs     11µs    88186.        0B     26.5
#>  6 worst               10          15  10.31µs   11.6µs    82234.        0B     32.9
#>  7 best                50          15  10.21µs     11µs    88341.        0B     35.4
#>  8 worst               50          15  14.04µs   14.8µs    65583.        0B     19.7
#>  9 best               100          15  11.73µs   12.5µs    77915.        0B     31.2
#> 10 worst              100          15  19.56µs   20.5µs    47360.        0B     19.0
#> 11 best                 3         100   9.66µs   10.8µs    89326.        0B     26.8
#> 12 worst                3         100   10.4µs   11.6µs    83004.        0B     24.9
#> 13 best                 5         100   9.47µs   10.6µs    91153.        0B     27.4
#> 14 worst                5         100  10.42µs   11.6µs    83352.        0B     33.4
#> 15 best                10         100   9.48µs   10.5µs    91446.        0B     27.4
#> 16 worst               10         100  11.82µs   12.9µs    74707.        0B     29.9
#> 17 best                50         100  10.71µs   11.8µs    81856.        0B     24.6
#> 18 worst               50         100   20.7µs   21.9µs    44410.        0B     17.8
#> 19 best               100         100  11.79µs   12.8µs    75574.        0B     30.2
#> 20 worst              100         100  32.37µs   33.6µs    28969.        0B     11.6
```
