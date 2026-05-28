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
#> 1 foo_S7(x)    6.68µs   8.21µs   113582.    10.8KB     22.7
#> 2 foo_S3(x)    2.29µs   2.69µs   333984.        0B      0  
#> 3 foo_S4(x)    2.54µs   2.86µs   329958.        0B     33.0

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
#> 1 bar_S7(x, y)   11.9µs  14.02µs    68943.        0B     20.7
#> 2 bar_S4(x, y)    6.5µs   7.34µs   132119.        0B     26.4
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
#>  1 best                 3          15   6.84µs   8.44µs   114653.        0B     22.9
#>  2 worst                3          15   7.02µs   8.55µs   113050.        0B     33.9
#>  3 best                 5          15   6.81µs   8.41µs   114653.        0B     34.4
#>  4 worst                5          15   7.04µs   8.62µs   112179.        0B     33.7
#>  5 best                10          15   6.89µs   8.49µs   114052.        0B     34.2
#>  6 worst               10          15   7.23µs   8.84µs   109515.        0B     21.9
#>  7 best                50          15   7.17µs   8.81µs   109842.        0B     33.0
#>  8 worst               50          15   8.61µs  10.32µs    93600.        0B     28.1
#>  9 best               100          15   7.72µs    9.4µs   102879.        0B     30.9
#> 10 worst              100          15  10.64µs  12.44µs    78321.        0B     23.5
#> 11 best                 3         100    6.9µs   8.47µs   113636.        0B     34.1
#> 12 worst                3         100   7.21µs   8.78µs   110132.        0B     33.0
#> 13 best                 5         100    6.9µs   8.39µs   115240.        0B     34.6
#> 14 worst                5         100   7.25µs   8.87µs   108116.        0B     32.4
#> 15 best                10         100    6.9µs    8.6µs   111653.        0B     33.5
#> 16 worst               10         100   7.55µs    9.2µs   104943.        0B     31.5
#> 17 best                50         100   7.13µs   8.76µs   109806.        0B     33.0
#> 18 worst               50         100  11.62µs  13.37µs    72591.        0B     21.8
#> 19 best               100         100    7.7µs   9.43µs   101794.        0B     30.5
#> 20 worst              100         100  17.02µs  18.86µs    51681.        0B     15.5
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
#>  1 best                 3          15   8.41µs   12.2µs    62474.        0B     25.0
#>  2 worst                3          15   8.79µs  10.64µs    89444.        0B     26.8
#>  3 best                 5          15   8.46µs   10.2µs    93954.        0B     28.2
#>  4 worst                5          15   8.89µs  10.79µs    88727.        0B     35.5
#>  5 best                10          15   8.51µs   10.4µs    92475.        0B     27.8
#>  6 worst               10          15   9.28µs   10.1µs    93672.        0B     37.5
#>  7 best                50          15   9.15µs   9.76µs    99379.        0B     39.8
#>  8 worst               50          15  12.12µs  12.75µs    76009.        0B     22.8
#>  9 best               100          15  10.06µs  10.66µs    91057.        0B     36.4
#> 10 worst              100          15  15.73µs  16.83µs    57691.        0B     23.1
#> 11 best                 3         100   8.53µs   9.67µs    98967.        0B     29.7
#> 12 worst                3         100   9.27µs  10.38µs    92206.        0B     36.9
#> 13 best                 5         100   8.42µs   9.49µs   101156.        0B     30.4
#> 14 worst                5         100    9.4µs  10.43µs    90161.        0B     36.1
#> 15 best                10         100   8.51µs   9.54µs   100501.        0B     40.2
#> 16 worst               10         100  10.55µs  11.55µs    83062.        0B     24.9
#> 17 best                50         100   9.44µs   10.5µs    89657.        0B     35.9
#> 18 worst               50         100  17.48µs  18.71µs    51934.        0B     20.8
#> 19 best               100         100  10.29µs  11.23µs    84927.        0B     34.0
#> 20 worst              100         100  26.83µs  28.25µs    34550.        0B     13.8
```
