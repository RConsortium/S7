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
#> 1 foo_S7(x)    6.35µs   7.22µs   130464.    10.9KB     26.1
#> 2 foo_S3(x)    2.62µs   2.95µs   307118.        0B     30.7
#> 3 foo_S4(x)    2.73µs   3.13µs   307046.        0B     30.7

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
#> 1 bar_S7(x, y)  14.07µs  15.46µs    62442.        0B     25.0
#> 2 bar_S4(x, y)   7.47µs   8.47µs   113406.        0B     22.7
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
#>  1 best                 3          15    6.4µs   7.33µs   129219.        0B    25.8 
#>  2 worst                3          15   6.77µs   7.74µs   125175.        0B    25.0 
#>  3 best                 5          15   6.39µs   7.27µs   132539.        0B    26.5 
#>  4 worst                5          15   6.96µs   7.86µs   123116.        0B    24.6 
#>  5 best                10          15   6.71µs   7.57µs   127892.        0B    25.6 
#>  6 worst               10          15   7.11µs   8.04µs   119471.        0B    23.9 
#>  7 best                50          15   6.92µs   7.78µs   124483.        0B    24.9 
#>  8 worst               50          15   9.08µs  10.16µs    94626.        0B    18.9 
#>  9 best               100          15   7.14µs   8.19µs   118000.        0B    23.6 
#> 10 worst              100          15  11.69µs   12.7µs    75141.        0B    15.0 
#> 11 best                 3         100    6.6µs   7.62µs   126060.        0B    25.2 
#> 12 worst                3         100   7.19µs   8.19µs   116628.        0B    23.3 
#> 13 best                 5         100   6.73µs   7.75µs   122960.        0B    24.6 
#> 14 worst                5         100   7.28µs    8.3µs   115294.        0B    23.1 
#> 15 best                10         100   6.69µs   7.62µs   125628.        0B    25.1 
#> 16 worst               10         100   7.73µs   8.74µs   108721.        0B    21.7 
#> 17 best                50         100   6.85µs   7.75µs   123329.        0B    24.7 
#> 18 worst               50         100  13.29µs  14.31µs    67751.        0B    13.6 
#> 19 best               100         100   7.28µs   8.22µs   116599.        0B    23.3 
#> 20 worst              100         100  21.03µs   22.1µs    43918.        0B     8.79
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
#>  1 best                 3          15   9.04µs   10.4µs    90585.        0B    27.2 
#>  2 worst                3          15   9.76µs   11.1µs    85168.        0B    25.6 
#>  3 best                 5          15   9.29µs   10.7µs    88408.        0B    26.5 
#>  4 worst                5          15  10.04µs   11.3µs    83606.        0B    25.1 
#>  5 best                10          15   9.47µs   10.9µs    86498.        0B    26.0 
#>  6 worst               10          15  10.29µs   11.7µs    81043.        0B    16.2 
#>  7 best                50          15   9.73µs   11.1µs    85755.        0B    25.7 
#>  8 worst               50          15     14µs   15.4µs    61877.        0B    18.6 
#>  9 best               100          15  10.15µs   11.6µs    81683.        0B    24.5 
#> 10 worst              100          15  19.08µs   20.4µs    46854.        0B    14.1 
#> 11 best                 3         100   9.48µs   10.8µs    87357.        0B    26.2 
#> 12 worst                3         100  10.62µs   11.9µs    79662.        0B    23.9 
#> 13 best                 5         100   9.45µs   10.7µs    87849.        0B    26.4 
#> 14 worst                5         100  10.44µs   11.6µs    82764.        0B    24.8 
#> 15 best                10         100    9.1µs    9.9µs    98459.        0B    29.5 
#> 16 worst               10         100  12.01µs   12.7µs    76400.        0B    22.9 
#> 17 best                50         100   9.81µs   10.7µs    90311.        0B    27.1 
#> 18 worst               50         100  21.61µs   22.6µs    43345.        0B    13.0 
#> 19 best               100         100  10.44µs   11.2µs    86340.        0B    25.9 
#> 20 worst              100         100  34.34µs   35.3µs    27701.        0B     8.31
```
