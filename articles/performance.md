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
#> 1 foo_S7(x)    7.95µs   9.88µs    96111.    10.8KB     28.8
#> 2 foo_S3(x)    2.48µs   2.88µs   314106.        0B      0  
#> 3 foo_S4(x)    2.65µs   3.23µs   296236.        0B     29.6

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
#> 1 bar_S7(x, y)  14.81µs  17.28µs    56321.        0B     22.5
#> 2 bar_S4(x, y)   6.94µs   8.18µs   118902.        0B     23.8
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
#>  1 best                 3          15   8.25µs   10.2µs    95469.        0B     28.6
#>  2 worst                3          15   8.31µs   10.2µs    93831.        0B     28.2
#>  3 best                 5          15   8.28µs   10.2µs    94772.        0B     28.4
#>  4 worst                5          15    8.4µs   10.4µs    93040.        0B     27.9
#>  5 best                10          15   8.34µs   10.2µs    94900.        0B     28.5
#>  6 worst               10          15   8.62µs   10.6µs    91412.        0B     27.4
#>  7 best                50          15   8.69µs   10.8µs    89710.        0B     35.9
#>  8 worst               50          15  10.53µs   12.5µs    77479.        0B     23.3
#>  9 best               100          15   9.44µs   11.5µs    84028.        0B     25.2
#> 10 worst              100          15  12.72µs   14.8µs    65467.        0B     26.2
#> 11 best                 3         100   8.27µs   10.4µs    91521.        0B     36.6
#> 12 worst                3         100   8.61µs   10.7µs    88779.        0B     26.6
#> 13 best                 5         100   8.34µs   10.4µs    90052.        0B     36.0
#> 14 worst                5         100   8.85µs   10.9µs    87182.        0B     26.2
#> 15 best                10         100   8.31µs   10.5µs    90141.        0B     36.1
#> 16 worst               10         100   9.09µs   11.1µs    85253.        0B     25.6
#> 17 best                50         100   8.74µs   10.8µs    87603.        0B     35.1
#> 18 worst               50         100  13.93µs   16.1µs    59386.        0B     17.8
#> 19 best               100         100    9.3µs   11.3µs    83935.        0B     33.6
#> 20 worst              100         100  20.14µs   22.4µs    43156.        0B     17.3
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
#>  1 best                 3          15   10.5µs   12.6µs    74312.        0B     37.2
#>  2 worst                3          15   10.8µs   11.9µs    81787.        0B     32.7
#>  3 best                 5          15   10.7µs   11.8µs    82505.        0B     41.3
#>  4 worst                5          15   11.2µs   12.3µs    79242.        0B     31.7
#>  5 best                10          15   10.8µs     12µs    81304.        0B     40.7
#>  6 worst               10          15   11.6µs   12.9µs    75207.        0B     30.1
#>  7 best                50          15   12.1µs   13.6µs    70517.        0B     35.3
#>  8 worst               50          15   15.1µs   16.6µs    58623.        0B     23.5
#>  9 best               100          15   13.1µs   15.3µs    59928.        0B     30.0
#> 10 worst              100          15   19.9µs   22.2µs    42668.        0B     17.1
#> 11 best                 3         100   11.1µs   13.2µs    69666.        0B     34.9
#> 12 worst                3         100   11.9µs   14.1µs    65445.        0B     26.2
#> 13 best                 5         100     11µs   13.1µs    70224.        0B     35.1
#> 14 worst                5         100   11.9µs     14µs    67066.        0B     26.8
#> 15 best                10         100     11µs   13.2µs    70510.        0B     35.3
#> 16 worst               10         100   13.3µs   15.4µs    61179.        0B     24.5
#> 17 best                50         100   12.2µs   14.4µs    64442.        0B     32.2
#> 18 worst               50         100   21.6µs   24.1µs    39433.        0B     15.8
#> 19 best               100         100   13.5µs   16.2µs    56935.        0B     39.9
#> 20 worst              100         100   32.8µs     36µs    26722.        0B     13.4
```
