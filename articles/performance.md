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
#> 1 foo_S7(x)    7.21µs   8.42µs   110674.    18.2KB     22.1
#> 2 foo_S3(x)    2.56µs   2.84µs   318640.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.14µs   306641.        0B     30.7

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
#> 1 bar_S7(x, y)  13.35µs  14.83µs    64885.        0B     26.0
#> 2 bar_S4(x, y)   7.38µs   8.24µs   117969.        0B     23.6
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
#>  1 best                 3          15   7.23µs   8.41µs   114407.        0B     34.3
#>  2 worst                3          15   7.43µs   8.72µs   110076.        0B     33.0
#>  3 best                 5          15   7.22µs   8.43µs   114288.        0B     22.9
#>  4 worst                5          15   7.54µs   8.71µs   111155.        0B     33.4
#>  5 best                10          15   7.28µs   8.56µs   112817.        0B     33.9
#>  6 worst               10          15    7.8µs   8.92µs   108185.        0B     32.5
#>  7 best                50          15   7.92µs   9.14µs   105724.        0B     31.7
#>  8 worst               50          15   9.73µs  10.97µs    88208.        0B     26.5
#>  9 best               100          15   8.47µs   9.72µs    99100.        0B     29.7
#> 10 worst              100          15   12.2µs  13.46µs    71995.        0B     21.6
#> 11 best                 3         100   7.53µs   8.75µs   110634.        0B     33.2
#> 12 worst                3         100   7.91µs   9.13µs   105784.        0B     31.7
#> 13 best                 5         100   7.52µs   8.77µs   110326.        0B     33.1
#> 14 worst                5         100   7.79µs   9.19µs   105056.        0B     31.5
#> 15 best                10         100   7.49µs    8.8µs   108716.        0B     21.7
#> 16 worst               10         100    8.1µs   9.56µs   100113.        0B     30.0
#> 17 best                50         100   7.97µs   9.34µs   102836.        0B     20.6
#> 18 worst               50         100  13.28µs  14.64µs    65935.        0B     19.8
#> 19 best               100         100   8.72µs  10.01µs    96014.        0B     28.8
#> 20 worst              100         100  19.67µs   21.2µs    45634.        0B     13.7
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
#>  1 best                 3          15   8.94µs   10.4µs    91254.        0B     27.4
#>  2 worst                3          15   9.39µs   10.9µs    87021.        0B     34.8
#>  3 best                 5          15   9.13µs   10.6µs    90283.        0B     27.1
#>  4 worst                5          15   9.62µs   11.1µs    85315.        0B     34.1
#>  5 best                10          15   9.38µs   10.8µs    88458.        0B     26.5
#>  6 worst               10          15  10.06µs   11.5µs    82984.        0B     33.2
#>  7 best                50          15   9.95µs   10.7µs    90784.        0B     27.2
#>  8 worst               50          15  13.52µs   14.2µs    68452.        0B     27.4
#>  9 best               100          15   11.2µs   11.9µs    81747.        0B     32.7
#> 10 worst              100          15  17.97µs   18.7µs    51897.        0B     20.8
#> 11 best                 3         100   9.43µs   10.5µs    91592.        0B     27.5
#> 12 worst                3         100  10.23µs   11.4µs    83903.        0B     25.2
#> 13 best                 5         100   9.17µs   10.3µs    92984.        0B     27.9
#> 14 worst                5         100  10.29µs   11.5µs    84002.        0B     25.2
#> 15 best                10         100   9.34µs   10.6µs    90282.        0B     36.1
#> 16 worst               10         100  11.53µs   12.9µs    73345.        0B     22.0
#> 17 best                50         100  10.58µs   11.9µs    80252.        0B     32.1
#> 18 worst               50         100  20.15µs   21.6µs    44676.        0B     13.4
#> 19 best               100         100  11.66µs   13.1µs    72438.        0B     29.0
#> 20 worst              100         100  31.64µs   33.2µs    29127.        0B     11.7
```
