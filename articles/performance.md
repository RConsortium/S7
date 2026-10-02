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
#> 1 foo_S7(x)    6.41µs   7.11µs   132568.    10.9KB     26.5
#> 2 foo_S3(x)    2.62µs    2.9µs   314665.        0B     31.5
#> 3 foo_S4(x)     2.8µs   3.19µs   303023.        0B     30.3

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
#> 1 bar_S7(x, y)  13.27µs   14.5µs    66175.        0B     26.5
#> 2 bar_S4(x, y)   7.34µs    8.3µs   116192.        0B     11.6
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
#>  1 best                 3          15   6.29µs   7.13µs   135071.        0B    27.0 
#>  2 worst                3          15   6.59µs   7.32µs   132078.        0B    26.4 
#>  3 best                 5          15   6.26µs   7.08µs   134976.        0B    27.0 
#>  4 worst                5          15   6.58µs   7.35µs   131444.        0B    26.3 
#>  5 best                10          15   6.35µs   7.19µs   131796.        0B    26.4 
#>  6 worst               10          15   6.86µs   7.63µs   126241.        0B    12.6 
#>  7 best                50          15   6.58µs   7.35µs   131269.        0B    26.3 
#>  8 worst               50          15   8.66µs   9.41µs   102508.        0B    20.5 
#>  9 best               100          15   6.84µs   7.62µs   126674.        0B    25.3 
#> 10 worst              100          15  10.86µs  11.62µs    83565.        0B    16.7 
#> 11 best                 3         100   6.39µs   7.23µs   131632.        0B    26.3 
#> 12 worst                3         100   6.92µs   7.75µs   123848.        0B    24.8 
#> 13 best                 5         100   6.49µs   7.32µs   130332.        0B    26.1 
#> 14 worst                5         100   6.91µs   7.73µs   123828.        0B    24.8 
#> 15 best                10         100   6.53µs   7.31µs   131045.        0B    26.2 
#> 16 worst               10         100   7.32µs   8.13µs   118379.        0B    11.8 
#> 17 best                50         100    6.6µs   7.37µs   130422.        0B    26.1 
#> 18 worst               50         100  12.03µs  12.83µs    75362.        0B    15.1 
#> 19 best               100         100   6.87µs   7.68µs   124296.        0B    24.9 
#> 20 worst              100         100  18.48µs  19.44µs    49958.        0B     9.99
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
#>  1 best                 3          15   8.77µs   9.94µs    95289.        0B    28.6 
#>  2 worst                3          15   9.22µs  10.33µs    92330.        0B    18.5 
#>  3 best                 5          15   8.71µs   9.95µs    94755.        0B    28.4 
#>  4 worst                5          15   9.33µs  10.52µs    90000.        0B    27.0 
#>  5 best                10          15   8.99µs  10.14µs    93270.        0B    28.0 
#>  6 worst               10          15   9.76µs  10.97µs    84877.        0B    17.0 
#>  7 best                50          15   9.27µs  10.42µs    90691.        0B    27.2 
#>  8 worst               50          15  13.23µs  14.36µs    66377.        0B    13.3 
#>  9 best               100          15   9.75µs  10.91µs    86796.        0B    26.0 
#> 10 worst              100          15   17.7µs  18.89µs    50798.        0B    10.2 
#> 11 best                 3         100   9.13µs  10.29µs    92944.        0B    18.6 
#> 12 worst                3         100   9.96µs  11.09µs    85657.        0B    25.7 
#> 13 best                 5         100   8.89µs  10.05µs    94100.        0B    28.2 
#> 14 worst                5         100   9.97µs  11.21µs    84882.        0B    25.5 
#> 15 best                10         100   8.63µs   9.93µs    96884.        0B    19.4 
#> 16 worst               10         100  10.96µs  11.74µs    82804.        0B    24.8 
#> 17 best                50         100   9.42µs  10.08µs    96302.        0B    19.3 
#> 18 worst               50         100  19.41µs  20.17µs    48185.        0B    14.5 
#> 19 best               100         100   9.92µs  10.57µs    91852.        0B    18.4 
#> 20 worst              100         100  30.18µs  31.01µs    31391.        0B     9.42
```
