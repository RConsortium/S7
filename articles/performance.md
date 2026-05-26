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
#> 1 foo_S7(x)    7.52µs   8.72µs   107351.    10.8KB     21.5
#> 2 foo_S3(x)    2.54µs   2.83µs   324614.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.13µs   309033.        0B     30.9

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
#> 1 bar_S7(x, y)  13.61µs  15.34µs    63167.        0B     25.3
#> 2 bar_S4(x, y)   7.27µs   8.19µs   118982.        0B     23.8
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
#>  1 best                 3          15   7.44µs   8.95µs   108170.        0B     21.6
#>  2 worst                3          15   7.72µs   9.24µs   104458.        0B     31.3
#>  3 best                 5          15   7.49µs   8.95µs   107972.        0B     32.4
#>  4 worst                5          15    7.9µs   9.27µs   104338.        0B     31.3
#>  5 best                10          15   7.74µs   9.09µs   104958.        0B     31.5
#>  6 worst               10          15   8.15µs   9.48µs   101682.        0B     20.3
#>  7 best                50          15   8.18µs   9.52µs   101916.        0B     30.6
#>  8 worst               50          15  10.14µs  11.59µs    83603.        0B     25.1
#>  9 best               100          15    8.9µs  10.32µs    93542.        0B     28.1
#> 10 worst              100          15     13µs  14.49µs    66947.        0B     20.1
#> 11 best                 3         100   7.66µs   9.23µs   104140.        0B     31.3
#> 12 worst                3         100   8.13µs   9.58µs   100959.        0B     30.3
#> 13 best                 5         100   7.68µs    9.3µs   104105.        0B     31.2
#> 14 worst                5         100   8.22µs   9.66µs    99867.        0B     30.0
#> 15 best                10         100   7.58µs   9.31µs   102137.        0B     30.7
#> 16 worst               10         100   8.53µs  10.04µs    94405.        0B     28.3
#> 17 best                50         100   8.21µs   9.71µs    98923.        0B     29.7
#> 18 worst               50         100  13.71µs  15.29µs    63171.        0B     12.6
#> 19 best               100         100   9.08µs  10.72µs    88709.        0B     26.6
#> 20 worst              100         100  20.64µs  22.46µs    43185.        0B     13.0
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
#>  1 best                 3          15   9.53µs   11.4µs    83593.        0B     25.1
#>  2 worst                3          15   9.87µs   11.7µs    80891.        0B     32.4
#>  3 best                 5          15   9.67µs   11.5µs    82852.        0B     24.9
#>  4 worst                5          15   10.1µs   11.9µs    79446.        0B     31.8
#>  5 best                10          15   9.62µs   11.3µs    83297.        0B     25.0
#>  6 worst               10          15  10.29µs   11.6µs    81670.        0B     32.7
#>  7 best                50          15  10.41µs   11.2µs    86798.        0B     34.7
#>  8 worst               50          15  14.23µs   15.1µs    64248.        0B     19.3
#>  9 best               100          15  11.59µs   12.6µs    77638.        0B     31.1
#> 10 worst              100          15  19.54µs   20.7µs    46672.        0B     18.7
#> 11 best                 3         100   9.67µs   10.9µs    87885.        0B     35.2
#> 12 worst                3         100  10.44µs   11.7µs    82416.        0B     24.7
#> 13 best                 5         100   9.32µs   10.7µs    89610.        0B     35.9
#> 14 worst                5         100  10.63µs   11.9µs    80889.        0B     24.3
#> 15 best                10         100   9.57µs   10.9µs    88182.        0B     35.3
#> 16 worst               10         100  11.81µs   13.1µs    73482.        0B     22.1
#> 17 best                50         100  10.84µs     12µs    80224.        0B     32.1
#> 18 worst               50         100  20.93µs   22.4µs    43361.        0B     13.0
#> 19 best               100         100  11.95µs   13.2µs    72793.        0B     29.1
#> 20 worst              100         100  32.64µs   34.2µs    28383.        0B     11.4
```
