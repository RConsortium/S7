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
#> 1 foo_S7(x)    6.93µs    8.2µs   113583.    18.2KB     22.7
#> 2 foo_S3(x)    2.54µs   2.91µs   313498.        0B      0  
#> 3 foo_S4(x)    2.76µs   3.23µs   296645.        0B     29.7

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
#> 1 bar_S7(x, y)  12.58µs   14.5µs    66850.        0B     26.8
#> 2 bar_S4(x, y)   6.93µs   8.03µs   118679.        0B     23.7
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
#>  1 best                 3          15   7.11µs   8.66µs   111656.        0B     22.3
#>  2 worst                3          15   7.38µs    8.9µs   108771.        0B     32.6
#>  3 best                 5          15   7.21µs   8.71µs   110981.        0B     33.3
#>  4 worst                5          15   7.42µs   8.83µs   109503.        0B     32.9
#>  5 best                10          15   7.24µs   8.69µs   111413.        0B     22.3
#>  6 worst               10          15   7.56µs   9.07µs   106590.        0B     32.0
#>  7 best                50          15   7.75µs   9.28µs   104277.        0B     31.3
#>  8 worst               50          15   9.34µs  10.85µs    89209.        0B     26.8
#>  9 best               100          15   8.35µs   9.89µs    97658.        0B     29.3
#> 10 worst              100          15  11.51µs  13.12µs    74091.        0B     22.2
#> 11 best                 3         100   7.29µs    8.8µs   109474.        0B     32.9
#> 12 worst                3         100   7.63µs   9.05µs   106329.        0B     31.9
#> 13 best                 5         100   7.32µs   8.72µs   110799.        0B     33.2
#> 14 worst                5         100   7.67µs   9.19µs   104326.        0B     31.3
#> 15 best                10         100   7.38µs   8.91µs   106766.        0B     32.0
#> 16 worst               10         100   8.03µs   9.54µs    99891.        0B     30.0
#> 17 best                50         100   7.72µs   9.24µs   102835.        0B     30.9
#> 18 worst               50         100  12.86µs  14.43µs    66742.        0B     20.0
#> 19 best               100         100   8.43µs  10.11µs    94473.        0B     28.4
#> 20 worst              100         100  19.06µs  20.81µs    46651.        0B     14.0
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
#>  1 best                 3          15   8.82µs   10.6µs    89730.        0B     26.9
#>  2 worst                3          15   9.13µs   10.9µs    86674.        0B     34.7
#>  3 best                 5          15   8.85µs   10.6µs    89352.        0B     26.8
#>  4 worst                5          15   9.29µs     11µs    86303.        0B     34.5
#>  5 best                10          15      9µs   10.7µs    88844.        0B     35.6
#>  6 worst               10          15    9.7µs   11.3µs    84641.        0B     25.4
#>  7 best                50          15  10.11µs   10.9µs    89469.        0B     35.8
#>  8 worst               50          15  13.27µs     14µs    69778.        0B     20.9
#>  9 best               100          15  11.27µs   12.1µs    80665.        0B     32.3
#> 10 worst              100          15  17.71µs   18.6µs    52582.        0B     21.0
#> 11 best                 3         100   9.08µs   10.2µs    94928.        0B     28.5
#> 12 worst                3         100   9.86µs   11.1µs    87683.        0B     26.3
#> 13 best                 5         100   8.98µs   10.2µs    94881.        0B     28.5
#> 14 worst                5         100   9.99µs   11.1µs    87126.        0B     26.1
#> 15 best                10         100      9µs   10.1µs    95247.        0B     28.6
#> 16 worst               10         100  11.28µs   12.5µs    77916.        0B     23.4
#> 17 best                50         100  10.51µs   11.6µs    83616.        0B     25.1
#> 18 worst               50         100  19.71µs     21µs    46346.        0B     18.5
#> 19 best               100         100  11.37µs   12.5µs    77357.        0B     38.7
#> 20 worst              100         100  30.66µs     32µs    30482.        0B     12.2
```
