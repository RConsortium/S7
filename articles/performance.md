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
#> 1 foo_S7(x)     7.2µs   8.34µs   112186.    18.2KB     22.4
#> 2 foo_S3(x)    2.54µs   2.81µs   323527.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.13µs   307287.        0B     30.7

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
#> 1 bar_S7(x, y)  13.29µs  14.82µs    65182.        0B     26.1
#> 2 bar_S4(x, y)   7.38µs   8.28µs   117979.        0B     23.6
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
#>  1 best                 3          15   7.27µs   8.51µs   114301.        0B     34.3
#>  2 worst                3          15   7.48µs   8.68µs   112088.        0B     22.4
#>  3 best                 5          15   7.33µs   8.66µs   112273.        0B     22.5
#>  4 worst                5          15   7.57µs   8.79µs   110537.        0B     33.2
#>  5 best                10          15   7.41µs   8.55µs   113387.        0B     34.0
#>  6 worst               10          15   7.75µs   8.97µs   107738.        0B     21.6
#>  7 best                50          15   7.96µs   9.28µs   104737.        0B     31.4
#>  8 worst               50          15    9.7µs  10.96µs    88592.        0B     17.7
#>  9 best               100          15   8.56µs   9.92µs    97546.        0B     29.3
#> 10 worst              100          15  11.74µs  13.07µs    74248.        0B     22.3
#> 11 best                 3         100   7.31µs   8.59µs   112568.        0B     33.8
#> 12 worst                3         100   7.71µs   8.94µs   108106.        0B     21.6
#> 13 best                 5         100   7.47µs   8.75µs   110727.        0B     22.1
#> 14 worst                5         100   7.72µs   9.05µs   107290.        0B     32.2
#> 15 best                10         100   7.45µs   8.68µs   110655.        0B     22.1
#> 16 worst               10         100   8.23µs   9.52µs   100219.        0B     30.1
#> 17 best                50         100   7.93µs   9.19µs   104686.        0B     20.9
#> 18 worst               50         100  12.94µs  14.16µs    68365.        0B     20.5
#> 19 best               100         100   8.64µs    9.9µs    96566.        0B     29.0
#> 20 worst              100         100  19.27µs  20.67µs    46979.        0B     14.1
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
#>  1 best                 3          15   8.95µs   10.4µs    91748.        0B     27.5
#>  2 worst                3          15   9.37µs   10.7µs    89727.        0B     35.9
#>  3 best                 5          15   9.23µs   10.6µs    90434.        0B     27.1
#>  4 worst                5          15   9.46µs   10.9µs    87658.        0B     26.3
#>  5 best                10          15   9.28µs   10.8µs    88961.        0B     26.7
#>  6 worst               10          15   9.92µs   11.5µs    83578.        0B     25.1
#>  7 best                50          15  10.18µs     11µs    86653.        0B     26.0
#>  8 worst               50          15  13.34µs   14.1µs    69521.        0B     20.9
#>  9 best               100          15  11.38µs   12.2µs    80063.        0B     24.0
#> 10 worst              100          15  17.53µs   18.5µs    52896.        0B     21.2
#> 11 best                 3         100    9.3µs   10.2µs    95306.        0B     38.1
#> 12 worst                3         100  10.07µs   11.3µs    84385.        0B     33.8
#> 13 best                 5         100   9.16µs   10.3µs    93461.        0B     28.0
#> 14 worst                5         100  10.19µs   11.3µs    85182.        0B     25.6
#> 15 best                10         100   9.19µs   10.3µs    92644.        0B     27.8
#> 16 worst               10         100  11.21µs   12.5µs    77277.        0B     23.2
#> 17 best                50         100   10.6µs   11.7µs    81599.        0B     32.7
#> 18 worst               50         100  19.63µs   20.9µs    46553.        0B     14.0
#> 19 best               100         100  11.69µs   12.8µs    75244.        0B     30.1
#> 20 worst              100         100  30.55µs   31.8µs    30634.        0B     12.3
```
