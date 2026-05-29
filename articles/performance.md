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
#> 1 foo_S7(x)    7.59µs   8.88µs   105237.    10.8KB     21.1
#> 2 foo_S3(x)    2.51µs   2.77µs   327154.        0B      0  
#> 3 foo_S4(x)    2.67µs   3.07µs   314526.        0B     31.5

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
#> 1 bar_S7(x, y)   13.6µs  15.35µs    62864.        0B     18.9
#> 2 bar_S4(x, y)    7.3µs   8.17µs   118334.        0B     23.7
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
#>  1 best                 3          15   7.57µs   8.91µs   108752.        0B     21.8
#>  2 worst                3          15   7.61µs   9.06µs   106288.        0B     31.9
#>  3 best                 5          15   7.62µs   9.06µs   106790.        0B     32.0
#>  4 worst                5          15   7.66µs   9.19µs   103338.        0B     31.0
#>  5 best                10          15   7.58µs   9.03µs   107340.        0B     32.2
#>  6 worst               10          15   7.91µs   9.42µs   102910.        0B     20.6
#>  7 best                50          15   8.21µs   9.55µs   101529.        0B     30.5
#>  8 worst               50          15   9.99µs  11.38µs    85248.        0B     25.6
#>  9 best               100          15   8.66µs  10.03µs    96363.        0B     28.9
#> 10 worst              100          15  12.73µs  14.24µs    68029.        0B     20.4
#> 11 best                 3         100   7.56µs      9µs   107545.        0B     32.3
#> 12 worst                3         100   8.04µs    9.4µs   102542.        0B     41.0
#> 13 best                 5         100   7.79µs   9.27µs   104381.        0B     31.3
#> 14 worst                5         100      8µs   9.49µs   101683.        0B     30.5
#> 15 best                10         100   7.68µs   9.23µs   103871.        0B     31.2
#> 16 worst               10         100   8.45µs  10.06µs    94824.        0B     19.0
#> 17 best                50         100    8.2µs   9.68µs    99844.        0B     30.0
#> 18 worst               50         100  13.82µs  15.25µs    63310.        0B     19.0
#> 19 best               100         100   8.88µs  10.35µs    92832.        0B     27.9
#> 20 worst              100         100   20.5µs  22.13µs    43861.        0B     13.2
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
#>  1 best                 3          15    9.3µs   10.9µs    87466.        0B     35.0
#>  2 worst                3          15   9.61µs   11.4µs    84018.        0B     25.2
#>  3 best                 5          15   9.42µs   11.1µs    85278.        0B     34.1
#>  4 worst                5          15   9.83µs   11.6µs    83045.        0B     24.9
#>  5 best                10          15   9.46µs   11.1µs    86300.        0B     34.5
#>  6 worst               10          15  10.01µs   11.3µs    85215.        0B     25.6
#>  7 best                50          15  10.24µs   11.1µs    87924.        0B     26.4
#>  8 worst               50          15  14.01µs   14.9µs    65559.        0B     26.2
#>  9 best               100          15  11.47µs   12.4µs    78886.        0B     39.5
#> 10 worst              100          15  19.07µs   20.4µs    47480.        0B     19.0
#> 11 best                 3         100   9.78µs   11.2µs    84958.        0B     25.5
#> 12 worst                3         100  10.44µs   12.1µs    79584.        0B     31.8
#> 13 best                 5         100   9.37µs   10.9µs    88038.        0B     26.4
#> 14 worst                5         100  10.56µs   12.1µs    78986.        0B     31.6
#> 15 best                10         100   9.49µs   11.1µs    86516.        0B     26.0
#> 16 worst               10         100   11.8µs   13.2µs    72749.        0B     29.1
#> 17 best                50         100  10.83µs   12.3µs    77855.        0B     31.2
#> 18 worst               50         100  20.74µs   22.3µs    43501.        0B     13.1
#> 19 best               100         100  11.67µs     13µs    73862.        0B     29.6
#> 20 worst              100         100  32.51µs   34.3µs    28296.        0B     11.3
```
