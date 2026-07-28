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
#> 1 foo_S7(x)    8.72µs  10.06µs    94260.    10.8KB     28.3
#> 2 foo_S3(x)    2.54µs   2.88µs   316863.        0B      0  
#> 3 foo_S4(x)    2.71µs   3.16µs   304705.        0B     30.5

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
#> 1 bar_S7(x, y)  15.68µs  17.48µs    55330.        0B     27.7
#> 2 bar_S4(x, y)   7.17µs   8.04µs   121088.        0B     12.1
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
#>  1 best                 3          15   8.71µs     10µs    96855.        0B     29.1
#>  2 worst                3          15   8.97µs   10.4µs    93400.        0B     37.4
#>  3 best                 5          15   8.77µs   10.1µs    96291.        0B     28.9
#>  4 worst                5          15   8.87µs   10.4µs    92643.        0B     37.1
#>  5 best                10          15   8.86µs   10.2µs    94457.        0B     28.3
#>  6 worst               10          15   9.24µs   10.7µs    90895.        0B     36.4
#>  7 best                50          15   9.29µs   10.6µs    91265.        0B     27.4
#>  8 worst               50          15  11.22µs   12.7µs    76286.        0B     22.9
#>  9 best               100          15    9.8µs   11.4µs    83425.        0B     25.0
#> 10 worst              100          15  13.92µs   15.6µs    61704.        0B     24.7
#> 11 best                 3         100   8.98µs   10.4µs    92893.        0B     27.9
#> 12 worst                3         100   9.32µs   10.8µs    88865.        0B     35.6
#> 13 best                 5         100   8.89µs   10.3µs    93565.        0B     28.1
#> 14 worst                5         100   9.29µs   10.5µs    91470.        0B     36.6
#> 15 best                10         100   8.93µs   10.2µs    94876.        0B     28.5
#> 16 worst               10         100   9.62µs   11.1µs    87235.        0B     34.9
#> 17 best                50         100   9.22µs   10.6µs    90721.        0B     27.2
#> 18 worst               50         100  14.62µs   16.1µs    60388.        0B     18.1
#> 19 best               100         100   9.95µs   11.3µs    85620.        0B     25.7
#> 20 worst              100         100  21.58µs   23.1µs    42030.        0B     16.8
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
#>  1 best                 3          15   11.5µs   13.6µs    70376.        0B     28.2
#>  2 worst                3          15   11.5µs   13.7µs    70273.        0B     35.2
#>  3 best                 5          15   11.2µs   12.1µs    80423.        0B     40.2
#>  4 worst                5          15   11.7µs   12.7µs    76497.        0B     30.6
#>  5 best                10          15   11.3µs   12.1µs    80874.        0B     40.5
#>  6 worst               10          15   12.1µs   12.9µs    75261.        0B     30.1
#>  7 best                50          15   12.1µs   13.2µs    73184.        0B     36.6
#>  8 worst               50          15   15.9µs   17.1µs    56763.        0B     22.7
#>  9 best               100          15   13.3µs   14.5µs    64939.        0B     32.5
#> 10 worst              100          15     21µs   22.1µs    44066.        0B     22.0
#> 11 best                 3         100   11.4µs   12.6µs    75106.        0B     37.6
#> 12 worst                3         100   12.3µs   13.4µs    71273.        0B     28.5
#> 13 best                 5         100   11.2µs   12.3µs    77736.        0B     38.9
#> 14 worst                5         100   12.2µs   13.3µs    72363.        0B     29.0
#> 15 best                10         100   11.5µs   12.7µs    75573.        0B     30.2
#> 16 worst               10         100   13.7µs   14.8µs    64369.        0B     32.2
#> 17 best                50         100   12.5µs   13.8µs    68279.        0B     27.3
#> 18 worst               50         100   22.6µs     24µs    39847.        0B     19.9
#> 19 best               100         100   13.7µs   15.1µs    62754.        0B     25.1
#> 20 worst              100         100   34.6µs   36.1µs    25975.        0B     13.0
```
