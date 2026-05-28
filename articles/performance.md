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
#> 1 foo_S7(x)    7.55µs   9.05µs   102526.    10.8KB     20.5
#> 2 foo_S3(x)    2.56µs   2.85µs   318900.        0B      0  
#> 3 foo_S4(x)    2.73µs   3.19µs   301204.        0B     30.1

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
#> 1 bar_S7(x, y)  13.86µs  15.81µs    60760.        0B     18.2
#> 2 bar_S4(x, y)   7.27µs   8.26µs   116926.        0B     23.4
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
#>  1 best                 3          15   7.74µs   9.34µs   103266.        0B     20.7
#>  2 worst                3          15   8.12µs   9.62µs   100174.        0B     30.1
#>  3 best                 5          15    7.7µs   9.23µs   104206.        0B     31.3
#>  4 worst                5          15    8.1µs   9.59µs   100697.        0B     30.2
#>  5 best                10          15   7.84µs   9.34µs   102817.        0B     30.9
#>  6 worst               10          15   8.29µs   9.76µs    99070.        0B     19.8
#>  7 best                50          15    8.3µs   9.75µs    98873.        0B     29.7
#>  8 worst               50          15  10.29µs  11.83µs    81426.        0B     24.4
#>  9 best               100          15   8.86µs  10.59µs    88185.        0B     26.5
#> 10 worst              100          15  13.05µs  14.76µs    65555.        0B     19.7
#> 11 best                 3         100   7.92µs   9.55µs   100511.        0B     30.2
#> 12 worst                3         100    8.4µs   9.91µs    97263.        0B     29.2
#> 13 best                 5         100   7.83µs   9.45µs   101796.        0B     30.5
#> 14 worst                5         100    8.4µs   9.93µs    96364.        0B     28.9
#> 15 best                10         100   7.93µs   9.58µs    99079.        0B     29.7
#> 16 worst               10         100   8.76µs  10.37µs    92086.        0B     27.6
#> 17 best                50         100   8.33µs   9.97µs    95454.        0B     28.6
#> 18 worst               50         100   13.9µs  15.63µs    61441.        0B     18.4
#> 19 best               100         100   9.16µs  10.83µs    88075.        0B     35.2
#> 20 worst              100         100  20.62µs   22.5µs    42755.        0B     12.8
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
#>  1 best                 3          15   9.61µs   11.4µs    83233.        0B    33.3 
#>  2 worst                3          15   9.96µs   11.7µs    81344.        0B    24.4 
#>  3 best                 5          15   9.61µs   11.4µs    83402.        0B    33.4 
#>  4 worst                5          15  10.19µs   11.9µs    80506.        0B    24.2 
#>  5 best                10          15   9.92µs   11.6µs    82206.        0B    24.7 
#>  6 worst               10          15  10.66µs   11.8µs    81069.        0B    32.4 
#>  7 best                50          15  10.52µs   11.5µs    82740.        0B    33.1 
#>  8 worst               50          15  14.47µs   15.4µs    63301.        0B    19.0 
#>  9 best               100          15  11.79µs   12.7µs    76480.        0B    30.6 
#> 10 worst              100          15  19.71µs     21µs    45884.        0B    18.4 
#> 11 best                 3         100   9.99µs   11.3µs    85331.        0B    25.6 
#> 12 worst                3         100  10.66µs     12µs    79622.        0B    31.9 
#> 13 best                 5         100   9.76µs   11.1µs    86578.        0B    26.0 
#> 14 worst                5         100  10.86µs   12.2µs    78925.        0B    31.6 
#> 15 best                10         100   9.92µs   11.2µs    85708.        0B    25.7 
#> 16 worst               10         100  12.18µs   13.5µs    71086.        0B    28.4 
#> 17 best                50         100     11µs   12.5µs    76334.        0B    30.5 
#> 18 worst               50         100  21.34µs   22.7µs    42419.        0B    12.7 
#> 19 best               100         100  12.07µs   13.4µs    71185.        0B    35.6 
#> 20 worst              100         100  33.02µs   34.7µs    27891.        0B     8.37
```
