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
#> 1 foo_S7(x)    7.36µs    8.9µs   105830.    10.8KB     31.8
#> 2 foo_S3(x)     2.5µs   2.88µs   315006.        0B     31.5
#> 3 foo_S4(x)    2.71µs   3.19µs   299092.        0B     29.9

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
#> 1 bar_S7(x, y)  12.74µs  14.98µs    64666.        0B     25.9
#> 2 bar_S4(x, y)   6.94µs   7.92µs   122806.        0B     24.6
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
#>  1 best                 3          15   7.51µs   8.99µs   107792.        0B     32.3
#>  2 worst                3          15    7.7µs   8.93µs   108497.        0B     32.6
#>  3 best                 5          15   7.51µs   8.97µs   107415.        0B     32.2
#>  4 worst                5          15    7.7µs   9.03µs   106893.        0B     32.1
#>  5 best                10          15   7.51µs   8.99µs   107808.        0B     32.4
#>  6 worst               10          15   8.01µs   9.36µs   103255.        0B     31.0
#>  7 best                50          15   8.06µs   9.59µs   100817.        0B     30.3
#>  8 worst               50          15   9.66µs  11.07µs    87583.        0B     26.3
#>  9 best               100          15   8.69µs  10.11µs    95797.        0B     38.3
#> 10 worst              100          15  12.13µs  13.72µs    70786.        0B     21.2
#> 11 best                 3         100   7.61µs    8.9µs   108250.        0B     32.5
#> 12 worst                3         100   7.95µs   9.19µs   104951.        0B     42.0
#> 13 best                 5         100   7.59µs   8.96µs   107304.        0B     32.2
#> 14 worst                5         100   7.92µs   9.28µs   102920.        0B     30.9
#> 15 best                10         100   7.58µs   8.94µs   106761.        0B     32.0
#> 16 worst               10         100   8.37µs   9.75µs    98206.        0B     29.5
#> 17 best                50         100   7.98µs   9.54µs   100544.        0B     30.2
#> 18 worst               50         100  13.22µs  14.79µs    65437.        0B     19.6
#> 19 best               100         100   8.96µs  10.62µs    90118.        0B     27.0
#> 20 worst              100         100  19.73µs  21.61µs    44871.        0B     13.5
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
#>  1 best                 3          15   9.37µs   11.3µs    84853.        0B     25.5
#>  2 worst                3          15   9.64µs   11.7µs    82396.        0B     33.0
#>  3 best                 5          15   9.24µs   11.2µs    85051.        0B     34.0
#>  4 worst                5          15    9.6µs   11.6µs    82423.        0B     33.0
#>  5 best                10          15   9.43µs   10.1µs    96188.        0B     38.5
#>  6 worst               10          15  10.16µs   10.9µs    89749.        0B     26.9
#>  7 best                50          15  10.58µs   11.3µs    86149.        0B     34.5
#>  8 worst               50          15  13.71µs   14.5µs    67560.        0B     20.3
#>  9 best               100          15  11.82µs   13.1µs    73440.        0B     29.4
#> 10 worst              100          15  18.36µs   19.8µs    48863.        0B     19.6
#> 11 best                 3         100   9.55µs   10.9µs    86873.        0B     34.8
#> 12 worst                3         100   10.4µs   11.6µs    82395.        0B     33.0
#> 13 best                 5         100   9.36µs   10.5µs    91117.        0B     27.3
#> 14 worst                5         100  10.48µs   11.8µs    81205.        0B     32.5
#> 15 best                10         100   9.54µs   10.7µs    89129.        0B     35.7
#> 16 worst               10         100  11.73µs   12.9µs    74346.        0B     29.8
#> 17 best                50         100  10.82µs   12.1µs    79519.        0B     31.8
#> 18 worst               50         100  20.09µs   21.6µs    45003.        0B     18.0
#> 19 best               100         100   11.9µs   13.1µs    73502.        0B     36.8
#> 20 worst              100         100  31.43µs   33.2µs    29370.        0B     17.6
```
