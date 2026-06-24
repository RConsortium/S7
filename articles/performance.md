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
#> 1 foo_S7(x)    7.74µs   8.99µs   104454.    10.8KB     31.3
#> 2 foo_S3(x)    2.49µs   2.81µs   322679.        0B     32.3
#> 3 foo_S4(x)    2.76µs   3.12µs   307302.        0B     30.7

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
#> 1 bar_S7(x, y)  13.77µs  15.57µs    61782.        0B     24.7
#> 2 bar_S4(x, y)   7.18µs   8.11µs   119852.        0B     24.0
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
#>  1 best                 3          15   7.79µs   9.17µs   104999.        0B     31.5
#>  2 worst                3          15   7.92µs   9.37µs   102813.        0B     30.9
#>  3 best                 5          15   7.79µs   9.24µs   104751.        0B     31.4
#>  4 worst                5          15   8.06µs   9.45µs   102279.        0B     30.7
#>  5 best                10          15   7.89µs   9.23µs   104918.        0B     31.5
#>  6 worst               10          15   8.07µs    9.5µs   101771.        0B     30.5
#>  7 best                50          15   8.44µs    9.8µs    98750.        0B     29.6
#>  8 worst               50          15  10.23µs  11.71µs    82568.        0B     24.8
#>  9 best               100          15   9.15µs  10.54µs    91637.        0B     27.5
#> 10 worst              100          15  12.99µs  14.58µs    66370.        0B     19.9
#> 11 best                 3         100   7.83µs   9.22µs   104603.        0B     31.4
#> 12 worst                3         100    8.2µs   9.61µs   100525.        0B     30.2
#> 13 best                 5         100   7.92µs   9.31µs   103069.        0B     30.9
#> 14 worst                5         100   8.29µs   9.87µs    96708.        0B     29.0
#> 15 best                10         100   7.88µs    9.4µs   101464.        0B     30.4
#> 16 worst               10         100   8.65µs  10.12µs    93799.        0B     28.1
#> 17 best                50         100   8.29µs   9.81µs    93667.        0B     28.1
#> 18 worst               50         100  13.81µs  15.42µs    61286.        0B     18.4
#> 19 best               100         100   8.86µs  10.63µs    88075.        0B     17.6
#> 20 worst              100         100  20.41µs  22.93µs    40113.        0B     12.0
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
#>  1 best                 3          15   9.59µs   11.7µs    72672.        0B     29.1
#>  2 worst                3          15   9.96µs   11.7µs    81801.        0B     32.7
#>  3 best                 5          15   9.48µs   11.5µs    82489.        0B     33.0
#>  4 worst                5          15    9.9µs   11.6µs    82498.        0B     33.0
#>  5 best                10          15   9.56µs   10.3µs    94857.        0B     38.0
#>  6 worst               10          15  10.36µs     11µs    87727.        0B     26.3
#>  7 best                50          15  10.64µs   11.4µs    85573.        0B     34.2
#>  8 worst               50          15  14.35µs   15.1µs    64328.        0B     19.3
#>  9 best               100          15   11.9µs   13.2µs    64673.        0B     25.9
#> 10 worst              100          15  19.58µs   21.1µs    44458.        0B     17.8
#> 11 best                 3         100   9.69µs   11.2µs    78564.        0B     31.4
#> 12 worst                3         100  10.62µs   11.7µs    82128.        0B     32.9
#> 13 best                 5         100   9.53µs   11.4µs    81492.        0B     32.6
#> 14 worst                5         100  10.74µs   12.1µs    78426.        0B     31.4
#> 15 best                10         100   9.72µs     11µs    85798.        0B     34.3
#> 16 worst               10         100  12.03µs   13.2µs    72523.        0B     21.8
#> 17 best                50         100   11.1µs   12.4µs    77117.        0B     30.9
#> 18 worst               50         100  20.88µs   22.3µs    43139.        0B     17.3
#> 19 best               100         100  12.04µs   13.3µs    71974.        0B     36.0
#> 20 worst              100         100  32.54µs   34.1µs    28545.        0B     11.4
```
