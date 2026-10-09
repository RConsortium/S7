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
#> 1 foo_S7(x)    3.58µs   4.27µs   221170.    10.9KB     44.2
#> 2 foo_S3(x)    1.57µs   1.83µs   490166.        0B     49.0
#> 3 foo_S4(x)    1.61µs   1.95µs   492134.        0B     49.2

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
#> 1 bar_S7(x, y)   7.55µs   8.67µs   111588.        0B     44.7
#> 2 bar_S4(x, y)   4.17µs   4.93µs   197856.        0B     39.6
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
#>  1 best                 3          15   3.62µs   4.39µs   219757.        0B     44.0
#>  2 worst                3          15   3.72µs    4.5µs   213414.        0B     42.7
#>  3 best                 5          15    3.6µs   4.35µs   222237.        0B     44.5
#>  4 worst                5          15   3.78µs   4.55µs   212551.        0B     42.5
#>  5 best                10          15    3.6µs   4.41µs   218703.        0B     43.7
#>  6 worst               10          15   3.81µs   4.68µs   205323.        0B     41.1
#>  7 best                50          15   3.75µs   4.55µs   212041.        0B     42.4
#>  8 worst               50          15   4.82µs   5.67µs   170064.        0B     34.0
#>  9 best               100          15   3.91µs   4.79µs   200664.        0B     40.1
#> 10 worst              100          15   6.22µs   7.09µs   137299.        0B     27.5
#> 11 best                 3         100   3.69µs   4.51µs   212140.        0B     42.4
#> 12 worst                3         100   3.89µs   4.73µs   201995.        0B     40.4
#> 13 best                 5         100   3.73µs   4.57µs   207520.        0B     41.5
#> 14 worst                5         100   3.95µs   4.77µs   199346.        0B     39.9
#> 15 best                10         100   3.63µs   4.44µs   216448.        0B     43.3
#> 16 worst               10         100   4.13µs   4.93µs   195654.        0B     39.1
#> 17 best                50         100   3.74µs   4.59µs   207545.        0B     41.5
#> 18 worst               50         100    7.2µs   8.07µs   119638.        0B     23.9
#> 19 best               100         100      4µs   4.81µs   201520.        0B     20.2
#> 20 worst              100         100  11.07µs     12µs    81303.        0B     16.3
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
#>  1 best                 3          15   5.09µs   6.34µs   145557.        0B    43.7 
#>  2 worst                3          15    5.3µs   6.57µs   142290.        0B    42.7 
#>  3 best                 5          15   5.12µs   6.47µs   144374.        0B    43.3 
#>  4 worst                5          15   5.38µs   6.64µs   141697.        0B    42.5 
#>  5 best                10          15    5.2µs   6.38µs   146469.        0B    44.0 
#>  6 worst               10          15   5.66µs   6.92µs   136083.        0B    40.8 
#>  7 best                50          15   5.41µs   6.65µs   141123.        0B    42.3 
#>  8 worst               50          15   7.53µs   8.87µs   107591.        0B    21.5 
#>  9 best               100          15   5.79µs   7.16µs   130654.        0B    39.2 
#> 10 worst              100          15  10.02µs  11.39µs    84049.        0B    25.2 
#> 11 best                 3         100   5.26µs   6.56µs   142143.        0B    42.7 
#> 12 worst                3         100   5.81µs   7.13µs   131326.        0B    39.4 
#> 13 best                 5         100   5.11µs   6.19µs   151980.        0B    45.6 
#> 14 worst                5         100    5.8µs   6.62µs   146367.        0B    43.9 
#> 15 best                10         100   5.17µs      6µs   160667.        0B    48.2 
#> 16 worst               10         100   6.69µs    7.7µs    95166.        0B    28.6 
#> 17 best                50         100   5.67µs   6.63µs   145524.        0B    43.7 
#> 18 worst               50         100  11.86µs  12.73µs    76990.        0B    23.1 
#> 19 best               100         100   5.81µs   6.69µs   144775.        0B    43.4 
#> 20 worst              100         100  19.18µs  20.29µs    48350.        0B     9.67
```
