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
#> 1 foo_S7(x)    7.61µs   8.91µs   105441.    10.8KB     31.6
#> 2 foo_S3(x)    2.56µs   2.83µs   322296.        0B     32.2
#> 3 foo_S4(x)    2.75µs   3.13µs   303354.        0B     30.3

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
#> 1 bar_S7(x, y)  13.97µs  15.63µs    61546.        0B     24.6
#> 2 bar_S4(x, y)   7.28µs   8.23µs   117411.        0B     23.5
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
#>  1 best                 3          15   7.88µs   9.24µs   104593.        0B     31.4
#>  2 worst                3          15   7.96µs   9.39µs   103454.        0B     31.0
#>  3 best                 5          15   7.97µs   9.25µs   104771.        0B     31.4
#>  4 worst                5          15   8.19µs   9.49µs   101812.        0B     30.6
#>  5 best                10          15   7.95µs   9.31µs   104166.        0B     31.3
#>  6 worst               10          15   8.42µs   9.68µs   100191.        0B     30.1
#>  7 best                50          15   8.51µs   9.83µs    98588.        0B     29.6
#>  8 worst               50          15  10.29µs  11.84µs    81731.        0B     24.5
#>  9 best               100          15   8.91µs  10.26µs    94546.        0B     37.8
#> 10 worst              100          15  13.05µs  14.48µs    66717.        0B     20.0
#> 11 best                 3         100   7.79µs   9.15µs   105308.        0B     31.6
#> 12 worst                3         100   8.18µs   9.51µs   101637.        0B     40.7
#> 13 best                 5         100   7.97µs   9.41µs   101960.        0B     30.6
#> 14 worst                5         100   8.39µs   9.77µs    98827.        0B     29.7
#> 15 best                10         100   7.96µs    9.3µs   103744.        0B     31.1
#> 16 worst               10         100   8.85µs  10.23µs    93919.        0B     28.2
#> 17 best                50         100   8.36µs   9.77µs    98580.        0B     29.6
#> 18 worst               50         100  13.89µs  15.27µs    63206.        0B     19.0
#> 19 best               100         100   9.12µs   10.7µs    89712.        0B     26.9
#> 20 worst              100         100  20.89µs  22.51µs    42906.        0B     12.9
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
#>  1 best                 3          15   9.78µs   11.5µs    83552.        0B     25.1
#>  2 worst                3          15  10.13µs   11.9µs    80482.        0B     32.2
#>  3 best                 5          15   9.69µs   11.4µs    83493.        0B     33.4
#>  4 worst                5          15   9.94µs   11.9µs    80938.        0B     32.4
#>  5 best                10          15   9.57µs   10.3µs    94237.        0B     37.7
#>  6 worst               10          15  10.44µs   11.2µs    87326.        0B     26.2
#>  7 best                50          15  10.65µs   11.4µs    85162.        0B     34.1
#>  8 worst               50          15  14.17µs     15µs    64716.        0B     19.4
#>  9 best               100          15  12.01µs   13.2µs    72917.        0B     29.2
#> 10 worst              100          15  19.81µs   21.2µs    45422.        0B     18.2
#> 11 best                 3         100   9.79µs   11.1µs    86127.        0B     34.5
#> 12 worst                3         100  10.75µs     12µs    79814.        0B     31.9
#> 13 best                 5         100   9.54µs   10.9µs    87437.        0B     35.0
#> 14 worst                5         100  10.75µs     12µs    80151.        0B     32.1
#> 15 best                10         100   9.83µs   11.1µs    86303.        0B     34.5
#> 16 worst               10         100  12.14µs   13.4µs    71849.        0B     28.8
#> 17 best                50         100  11.07µs   12.4µs    77088.        0B     30.8
#> 18 worst               50         100  20.96µs   22.3µs    43158.        0B     17.3
#> 19 best               100         100  12.12µs   13.4µs    71523.        0B     35.8
#> 20 worst              100         100  32.57µs   34.3µs    28195.        0B     14.1
```
