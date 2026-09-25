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
#> 1 foo_S7(x)    6.23µs   6.86µs   137108.    10.8KB     13.7
#> 2 foo_S3(x)     2.6µs   2.84µs   319667.        0B     32.0
#> 3 foo_S4(x)    2.75µs   3.06µs   312267.        0B     31.2

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
#> 1 bar_S7(x, y)  12.91µs  13.97µs    68856.        0B     20.7
#> 2 bar_S4(x, y)   7.16µs   7.83µs   124157.        0B     24.8
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
#>  1 best                 3          15    6.2µs   6.79µs   142937.        0B    28.6 
#>  2 worst                3          15   6.41µs   6.93µs   139660.        0B    27.9 
#>  3 best                 5          15   6.24µs   6.75µs   143559.        0B    28.7 
#>  4 worst                5          15   6.43µs   6.95µs   139351.        0B    27.9 
#>  5 best                10          15   6.17µs   6.74µs   142584.        0B    14.3 
#>  6 worst               10          15   6.52µs    7.1µs   136145.        0B    27.2 
#>  7 best                50          15   6.37µs      7µs   138635.        0B    27.7 
#>  8 worst               50          15   8.38µs   8.94µs   108711.        0B    10.9 
#>  9 best               100          15   6.68µs   7.38µs   130944.        0B    26.2 
#> 10 worst              100          15  10.61µs   11.3µs    86058.        0B     8.61
#> 11 best                 3         100   6.28µs   6.95µs   139065.        0B    27.8 
#> 12 worst                3         100   6.62µs   7.27µs   132662.        0B    26.5 
#> 13 best                 5         100   6.29µs   6.99µs   137758.        0B    27.6 
#> 14 worst                5         100   6.65µs   7.38µs   115134.        0B    23.0 
#> 15 best                10         100   6.31µs   6.89µs   139933.        0B    14.0 
#> 16 worst               10         100   7.16µs   7.78µs   124719.        0B    24.9 
#> 17 best                50         100   6.44µs   7.06µs   136828.        0B    13.7 
#> 18 worst               50         100  12.02µs  12.77µs    76089.        0B     7.61
#> 19 best               100         100   6.93µs   7.66µs   126449.        0B    12.6 
#> 20 worst              100         100  18.52µs  19.38µs    50120.        0B    10.0
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
#>  1 best                 3          15   8.73µs   9.68µs    99122.        0B    29.7 
#>  2 worst                3          15   8.98µs   9.98µs    96391.        0B    19.3 
#>  3 best                 5          15   8.78µs   9.67µs    97942.        0B    29.4 
#>  4 worst                5          15   9.27µs  10.19µs    93763.        0B    18.8 
#>  5 best                10          15   8.82µs   9.75µs    98508.        0B    19.7 
#>  6 worst               10          15   9.62µs  10.57µs    90553.        0B    27.2 
#>  7 best                50          15    9.2µs  10.09µs    94079.        0B    28.2 
#>  8 worst               50          15  12.88µs  13.98µs    68684.        0B    13.7 
#>  9 best               100          15   9.68µs  10.61µs    89622.        0B    26.9 
#> 10 worst              100          15  17.75µs  18.82µs    47427.        0B     9.49
#> 11 best                 3         100   8.93µs     10µs    94770.        0B    28.4 
#> 12 worst                3         100   9.82µs  10.77µs    88908.        0B    17.8 
#> 13 best                 5         100   8.84µs   9.79µs    97985.        0B    19.6 
#> 14 worst                5         100   9.97µs  10.92µs    87134.        0B    26.1 
#> 15 best                10         100   8.84µs    9.8µs    97720.        0B    19.5 
#> 16 worst               10         100  11.12µs   12.1µs    78708.        0B    23.6 
#> 17 best                50         100   9.06µs   10.2µs    93487.        0B    18.7 
#> 18 worst               50         100  19.38µs  19.99µs    48816.        0B    14.6 
#> 19 best               100         100   9.77µs  10.28µs    94858.        0B    19.0 
#> 20 worst              100         100  30.03µs  30.71µs    31846.        0B     9.56
```
