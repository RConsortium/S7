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
#> 1 foo_S7(x)    7.18µs   8.39µs   111510.    18.2KB     22.3
#> 2 foo_S3(x)    2.57µs   2.83µs   320828.        0B      0  
#> 3 foo_S4(x)    2.77µs   3.16µs   305995.        0B     30.6

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
#> 1 bar_S7(x, y)  13.25µs  14.79µs    65428.        0B     26.2
#> 2 bar_S4(x, y)   7.48µs   8.31µs   117820.        0B     23.6
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
#>  1 best                 3          15   7.25µs   8.53µs   113674.        0B     34.1
#>  2 worst                3          15    7.4µs   8.72µs   111496.        0B     22.3
#>  3 best                 5          15    7.4µs   8.77µs   110818.        0B     22.2
#>  4 worst                5          15   7.67µs   8.98µs   107828.        0B     32.4
#>  5 best                10          15   7.51µs   8.85µs   109424.        0B     32.8
#>  6 worst               10          15   7.74µs   9.18µs   105305.        0B     21.1
#>  7 best                50          15   7.95µs   9.41µs   103122.        0B     30.9
#>  8 worst               50          15   9.67µs  11.05µs    87980.        0B     17.6
#>  9 best               100          15   8.74µs  10.08µs    95797.        0B     28.7
#> 10 worst              100          15  11.95µs  13.21µs    73549.        0B     22.1
#> 11 best                 3         100   7.61µs   8.83µs   109514.        0B     32.9
#> 12 worst                3         100   7.83µs   9.09µs   106839.        0B     21.4
#> 13 best                 5         100   7.67µs   8.95µs   108365.        0B     21.7
#> 14 worst                5         100   7.89µs   9.28µs   104388.        0B     31.3
#> 15 best                10         100   7.54µs   8.82µs   108618.        0B     21.7
#> 16 worst               10         100   8.23µs   9.63µs    98500.        0B     29.6
#> 17 best                50         100   7.94µs    9.4µs   101723.        0B     20.3
#> 18 worst               50         100     13µs  14.43µs    66455.        0B     19.9
#> 19 best               100         100   8.64µs  10.16µs    92261.        0B     27.7
#> 20 worst              100         100  19.03µs  20.56µs    47247.        0B     14.2
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
#>  1 best                 3          15   9.07µs   10.5µs    91082.        0B     27.3
#>  2 worst                3          15   9.46µs   10.8µs    87725.        0B     35.1
#>  3 best                 5          15   9.23µs   10.6µs    89670.        0B     26.9
#>  4 worst                5          15   9.64µs   11.2µs    84283.        0B     25.3
#>  5 best                10          15   9.36µs     11µs    86034.        0B     25.8
#>  6 worst               10          15  10.09µs   11.8µs    80967.        0B     24.3
#>  7 best                50          15  10.25µs   11.2µs    85514.        0B     25.7
#>  8 worst               50          15  13.29µs   14.1µs    69396.        0B     20.8
#>  9 best               100          15  11.63µs   12.4µs    78406.        0B     23.5
#> 10 worst              100          15   17.8µs   18.7µs    52440.        0B     21.0
#> 11 best                 3         100   9.34µs   10.2µs    95214.        0B     38.1
#> 12 worst                3         100   10.1µs   11.4µs    84222.        0B     33.7
#> 13 best                 5         100   9.26µs   10.5µs    91589.        0B     27.5
#> 14 worst                5         100  10.23µs   11.6µs    82533.        0B     24.8
#> 15 best                10         100   9.43µs   10.6µs    89636.        0B     26.9
#> 16 worst               10         100  11.44µs   12.7µs    75747.        0B     22.7
#> 17 best                50         100  10.64µs     12µs    79881.        0B     32.0
#> 18 worst               50         100  19.57µs     21µs    46276.        0B     13.9
#> 19 best               100         100  11.67µs   12.9µs    74349.        0B     29.8
#> 20 worst              100         100  30.36µs   31.7µs    30698.        0B     12.3
```
