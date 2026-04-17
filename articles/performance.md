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
#> 1 foo_S7(x)    7.21µs   8.39µs   111099.    18.2KB     22.2
#> 2 foo_S3(x)    2.58µs   2.82µs   323427.        0B      0  
#> 3 foo_S4(x)    2.79µs   3.16µs   305705.        0B     30.6

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
#> 1 bar_S7(x, y)  13.05µs  14.64µs    65916.        0B     26.4
#> 2 bar_S4(x, y)   7.26µs   8.11µs   119664.        0B     23.9
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
#>  1 best                 3          15   7.13µs   8.45µs   114652.        0B     22.9
#>  2 worst                3          15    7.5µs   8.74µs   110580.        0B     33.2
#>  3 best                 5          15   7.29µs   8.48µs   114004.        0B     34.2
#>  4 worst                5          15   7.58µs   8.81µs   110028.        0B     33.0
#>  5 best                10          15   7.53µs   8.72µs   110572.        0B     22.1
#>  6 worst               10          15   7.82µs   9.04µs   106826.        0B     32.1
#>  7 best                50          15   7.96µs   9.29µs   103932.        0B     31.2
#>  8 worst               50          15   9.74µs   11.1µs    87442.        0B     17.5
#>  9 best               100          15    8.5µs   9.71µs    99548.        0B     29.9
#> 10 worst              100          15   11.9µs  13.28µs    73135.        0B     21.9
#> 11 best                 3         100   7.08µs   8.49µs   113786.        0B     34.1
#> 12 worst                3         100   7.63µs   9.04µs   106491.        0B     32.0
#> 13 best                 5         100    7.3µs   8.64µs   111206.        0B     33.4
#> 14 worst                5         100   7.83µs   9.13µs   105579.        0B     31.7
#> 15 best                10         100   7.53µs   8.83µs   107577.        0B     32.3
#> 16 worst               10         100   8.18µs   9.62µs    99314.        0B     29.8
#> 17 best                50         100   7.88µs   9.22µs   103360.        0B     31.0
#> 18 worst               50         100  13.06µs  14.49µs    66445.        0B     19.9
#> 19 best               100         100   8.57µs   9.94µs    96244.        0B     28.9
#> 20 worst              100         100  19.39µs  21.14µs    44524.        0B     13.4
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
#>  1 best                 3          15   9.05µs   10.4µs    91349.        0B     36.6
#>  2 worst                3          15   9.36µs   10.8µs    87908.        0B     26.4
#>  3 best                 5          15   9.11µs   10.4µs    90483.        0B     36.2
#>  4 worst                5          15   9.53µs     11µs    85795.        0B     25.7
#>  5 best                10          15   9.17µs   10.7µs    87661.        0B     35.1
#>  6 worst               10          15   9.95µs   11.3µs    84873.        0B     25.5
#>  7 best                50          15  10.21µs   10.9µs    89247.        0B     35.7
#>  8 worst               50          15  13.34µs   14.2µs    68448.        0B     20.5
#>  9 best               100          15  11.26µs   12.1µs    80332.        0B     32.1
#> 10 worst              100          15  18.21µs   19.1µs    50885.        0B     20.4
#> 11 best                 3         100   9.38µs   10.4µs    92265.        0B     27.7
#> 12 worst                3         100  10.06µs   11.2µs    86095.        0B     34.5
#> 13 best                 5         100   9.04µs   10.2µs    94463.        0B     28.3
#> 14 worst                5         100  10.05µs   11.3µs    85772.        0B     34.3
#> 15 best                10         100   9.32µs   10.4µs    92534.        0B     27.8
#> 16 worst               10         100  11.62µs   12.7µs    76019.        0B     30.4
#> 17 best                50         100  10.37µs   11.7µs    81733.        0B     32.7
#> 18 worst               50         100  20.01µs   21.2µs    45790.        0B     13.7
#> 19 best               100         100  11.48µs   12.6µs    76777.        0B     30.7
#> 20 worst              100         100  31.25µs   32.6µs    29860.        0B     11.9
```
