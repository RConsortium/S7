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
#> 1 foo_S7(x)    7.18µs   8.22µs   113413.    18.2KB     22.7
#> 2 foo_S3(x)    2.58µs   2.84µs   319765.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.13µs   304680.        0B     30.5

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
#> 1 bar_S7(x, y)  13.27µs  14.86µs    65113.        0B     26.1
#> 2 bar_S4(x, y)   7.38µs   8.19µs   119111.        0B     23.8
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
#>  1 best                 3          15    7.3µs   8.52µs   113831.        0B     34.2
#>  2 worst                3          15   7.51µs   8.54µs   113856.        0B     34.2
#>  3 best                 5          15   7.36µs   8.35µs   116474.        0B     23.3
#>  4 worst                5          15   7.66µs   9.05µs   105813.        0B     31.8
#>  5 best                10          15   7.37µs   8.64µs   111610.        0B     33.5
#>  6 worst               10          15   7.88µs   9.02µs   107392.        0B     32.2
#>  7 best                50          15   7.96µs   9.17µs   105645.        0B     31.7
#>  8 worst               50          15   9.76µs  11.03µs    87705.        0B     26.3
#>  9 best               100          15   8.63µs   9.82µs    98826.        0B     29.7
#> 10 worst              100          15   12.3µs  13.49µs    72063.        0B     21.6
#> 11 best                 3         100   7.49µs   8.53µs   113816.        0B     34.2
#> 12 worst                3         100   7.74µs   8.73µs   110834.        0B     33.3
#> 13 best                 5         100   7.51µs   8.61µs   112476.        0B     33.8
#> 14 worst                5         100   7.87µs    8.9µs   109093.        0B     32.7
#> 15 best                10         100   7.45µs   8.59µs   112263.        0B     22.5
#> 16 worst               10         100   8.31µs   9.41µs   102915.        0B     30.9
#> 17 best                50         100   7.84µs   9.02µs   107322.        0B     21.5
#> 18 worst               50         100  13.27µs  14.55µs    66785.        0B     20.0
#> 19 best               100         100    8.7µs   9.89µs    98013.        0B     29.4
#> 20 worst              100         100   19.8µs  21.06µs    46350.        0B     13.9
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
#>  1 best                 3          15   9.11µs   10.4µs    92696.        0B     27.8
#>  2 worst                3          15   9.47µs   10.7µs    89658.        0B     35.9
#>  3 best                 5          15   9.24µs   10.5µs    91423.        0B     27.4
#>  4 worst                5          15   9.64µs   10.9µs    87640.        0B     35.1
#>  5 best                10          15   9.28µs   10.7µs    90372.        0B     27.1
#>  6 worst               10          15     10µs   11.3µs    85430.        0B     34.2
#>  7 best                50          15  10.15µs   10.8µs    90794.        0B     27.2
#>  8 worst               50          15  13.56µs   14.2µs    68454.        0B     27.4
#>  9 best               100          15  11.27µs   11.9µs    81460.        0B     32.6
#> 10 worst              100          15  18.15µs   18.9µs    51730.        0B     25.9
#> 11 best                 3         100   9.31µs   10.2µs    94051.        0B     28.2
#> 12 worst                3         100   9.98µs     11µs    86047.        0B     34.4
#> 13 best                 5         100   9.09µs   10.1µs    95495.        0B     28.7
#> 14 worst                5         100  10.19µs   11.2µs    86021.        0B     25.8
#> 15 best                10         100   9.24µs   10.3µs    93754.        0B     37.5
#> 16 worst               10         100  11.48µs   12.6µs    76565.        0B     23.0
#> 17 best                50         100   10.5µs   11.7µs    82497.        0B     33.0
#> 18 worst               50         100  20.18µs   21.2µs    45754.        0B     13.7
#> 19 best               100         100  11.45µs   12.7µs    75916.        0B     30.4
#> 20 worst              100         100  31.47µs   32.8µs    29607.        0B     11.8
```
