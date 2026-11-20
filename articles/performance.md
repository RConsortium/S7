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
#> 1 foo_S7(x)    3.94µs   4.28µs   217830.    10.9KB     43.6
#> 2 foo_S3(x)    1.52µs   1.68µs   532347.        0B     53.2
#> 3 foo_S4(x)    1.66µs   1.81µs   526620.        0B     52.7

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
#> 1 bar_S7(x, y)   7.93µs   8.63µs   111677.        0B     44.7
#> 2 bar_S4(x, y)   4.55µs    4.9µs   198992.        0B     39.8
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
#>  1 best                 3          15   3.91µs   4.23µs   227947.        0B     45.6
#>  2 worst                3          15   4.05µs   4.38µs   221089.        0B     44.2
#>  3 best                 5          15   3.91µs   4.22µs   228914.        0B     45.8
#>  4 worst                5          15   4.14µs   4.44µs   217015.        0B     43.4
#>  5 best                10          15   3.94µs   4.24µs   228188.        0B     45.6
#>  6 worst               10          15   4.26µs   4.57µs   212198.        0B     42.4
#>  7 best                50          15   4.07µs   4.39µs   221000.        0B     44.2
#>  8 worst               50          15   5.57µs    5.9µs   165220.        0B     33.1
#>  9 best               100          15   4.28µs   4.59µs   211749.        0B     42.4
#> 10 worst              100          15   7.29µs   7.65µs   127810.        0B     25.6
#> 11 best                 3         100   4.06µs   4.38µs   218476.        0B     43.7
#> 12 worst                3         100   4.42µs   4.74µs   203611.        0B     40.7
#> 13 best                 5         100   4.07µs   4.38µs   219001.        0B     43.8
#> 14 worst                5         100   4.45µs   4.76µs   202663.        0B     40.5
#> 15 best                10         100   4.02µs   4.32µs   223072.        0B     44.6
#> 16 worst               10         100   4.79µs    5.1µs   189389.        0B     37.9
#> 17 best                50         100   4.06µs   4.35µs   221810.        0B     44.4
#> 18 worst               50         100   8.34µs   9.95µs   100611.        0B     20.1
#> 19 best               100         100   4.42µs   4.73µs   204399.        0B     40.9
#> 20 worst              100         100  14.57µs  17.35µs    58350.        0B     11.7
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
#>  1 best                 3          15    4.8µs   5.21µs   183112.        0B     54.9
#>  2 worst                3          15   5.06µs   5.46µs   174837.        0B     52.5
#>  3 best                 5          15    4.8µs   5.18µs   182701.        0B     54.8
#>  4 worst                5          15   5.16µs   5.55µs   172891.        0B     51.9
#>  5 best                10          15    4.8µs   5.24µs   182416.        0B     54.7
#>  6 worst               10          15   5.43µs   5.86µs   163406.        0B     49.0
#>  7 best                50          15   5.07µs   5.48µs   174027.        0B     52.2
#>  8 worst               50          15   7.47µs   7.89µs   122751.        0B     36.8
#>  9 best               100          15    5.4µs   5.82µs   165820.        0B     33.2
#> 10 worst              100          15  10.48µs  11.11µs    85130.        0B     25.5
#> 11 best                 3         100   5.03µs   5.45µs   175359.        0B     52.6
#> 12 worst                3         100   5.78µs   6.23µs   152962.        0B     45.9
#> 13 best                 5         100   4.82µs   5.24µs   182241.        0B     54.7
#> 14 worst                5         100   5.76µs   6.11µs   157118.        0B     47.1
#> 15 best                10         100    4.8µs   5.06µs   189921.        0B     57.0
#> 16 worst               10         100   6.92µs   7.14µs   136865.        0B     41.1
#> 17 best                50         100   5.29µs   5.67µs   163273.        0B     49.0
#> 18 worst               50         100  14.37µs  14.75µs    66581.        0B     20.0
#> 19 best               100         100   5.52µs   5.74µs   168316.        0B     50.5
#> 20 worst              100         100   23.7µs  24.22µs    40593.        0B     12.2
```
