# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text := new_class(parent = class_character)
Number := new_class(parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 := new_generic("x")
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
#> 1 foo_S7(x)    7.14µs   8.48µs   110377.    10.8KB     33.1
#> 2 foo_S3(x)    2.47µs   2.81µs   326380.        0B     32.6
#> 3 foo_S4(x)    2.65µs   3.14µs   304893.        0B     30.5

bar_S7 := new_generic(c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)  12.82µs  14.71µs    66097.        0B     26.4
#> 2 bar_S4(x, y)   6.96µs   7.91µs   123075.        0B     24.6
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
    Text := new_class(parent = class_character)
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
    foo_S7 := new_generic("x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic("x")
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
#>  1 best                 3          15   7.25µs   8.71µs   111330.        0B     33.4
#>  2 worst                3          15    7.5µs   8.75µs   110885.        0B     33.3
#>  3 best                 5          15   7.27µs   8.62µs   111890.        0B     33.6
#>  4 worst                5          15   7.56µs    8.8µs   110182.        0B     33.1
#>  5 best                10          15   7.39µs   8.62µs   112443.        0B     33.7
#>  6 worst               10          15   7.75µs   8.95µs   108527.        0B     32.6
#>  7 best                50          15   7.93µs   9.21µs   105326.        0B     31.6
#>  8 worst               50          15   9.46µs  10.83µs    89746.        0B     26.9
#>  9 best               100          15   8.57µs   9.86µs    98624.        0B     29.6
#> 10 worst              100          15  11.64µs  13.02µs    74775.        0B     22.4
#> 11 best                 3         100   7.47µs   8.76µs   110404.        0B     33.1
#> 12 worst                3         100    7.8µs   8.93µs   108587.        0B     32.6
#> 13 best                 5         100   7.34µs   8.72µs   110130.        0B     33.0
#> 14 worst                5         100   7.77µs   9.09µs   105358.        0B     31.6
#> 15 best                10         100   7.47µs   8.83µs   108709.        0B     32.6
#> 16 worst               10         100    8.1µs   9.37µs   102601.        0B     30.8
#> 17 best                50         100   7.88µs   9.35µs   102560.        0B     30.8
#> 18 worst               50         100  12.93µs  14.42µs    67203.        0B     20.2
#> 19 best               100         100   8.58µs   9.95µs    96929.        0B     19.4
#> 20 worst              100         100  19.03µs  20.77µs    46727.        0B     14.0
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text := new_class(parent = class_character)
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
    foo_S7 := new_generic(c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic(c("x", "y"))
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
#>  1 best                 3          15   9.11µs   10.9µs    88094.        0B     35.3
#>  2 worst                3          15   9.46µs  11.04µs    86810.        0B     34.7
#>  3 best                 5          15   9.17µs  10.73µs    89730.        0B     35.9
#>  4 worst                5          15    9.4µs   10.7µs    89935.        0B     36.0
#>  5 best                10          15   9.05µs   9.77µs    99767.        0B     39.9
#>  6 worst               10          15   9.88µs   10.5µs    93161.        0B     28.0
#>  7 best                50          15  10.13µs  10.81µs    90577.        0B     36.2
#>  8 worst               50          15  13.46µs  14.17µs    69124.        0B     20.7
#>  9 best               100          15  11.45µs  12.48µs    77656.        0B     31.1
#> 10 worst              100          15     18µs  19.17µs    50758.        0B     20.3
#> 11 best                 3         100    9.3µs  10.34µs    93499.        0B     37.4
#> 12 worst                3         100  10.13µs  11.15µs    86744.        0B     34.7
#> 13 best                 5         100   9.16µs  10.23µs    93700.        0B     37.5
#> 14 worst                5         100  10.21µs  11.45µs    83925.        0B     33.6
#> 15 best                10         100   9.21µs  10.49µs    91381.        0B     36.6
#> 16 worst               10         100  11.47µs  12.63µs    76249.        0B     22.9
#> 17 best                50         100  10.53µs  11.71µs    82156.        0B     32.9
#> 18 worst               50         100  20.41µs  21.85µs    44434.        0B     17.8
#> 19 best               100         100  11.62µs  12.64µs    76192.        0B     38.1
#> 20 worst              100         100  30.95µs  32.43µs    30134.        0B     12.1
```
