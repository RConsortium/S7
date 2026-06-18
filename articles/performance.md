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
#> 1 foo_S7(x)    7.82µs   9.06µs   103970.    10.8KB     31.2
#> 2 foo_S3(x)    2.61µs   2.89µs   315206.        0B     31.5
#> 3 foo_S4(x)    2.83µs   3.19µs   302618.        0B     30.3

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
#> 1 bar_S7(x, y)  14.21µs  15.95µs    60532.        0B     24.2
#> 2 bar_S4(x, y)   7.41µs   8.23µs   118025.        0B     23.6
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
#>  1 best                 3          15   7.92µs   9.17µs   105703.        0B     31.7
#>  2 worst                3          15   8.06µs   9.43µs   102808.        0B     30.9
#>  3 best                 5          15    7.8µs   9.18µs   105399.        0B     31.6
#>  4 worst                5          15   7.99µs   9.43µs   102831.        0B     30.9
#>  5 best                10          15   7.97µs   9.32µs   103748.        0B     31.1
#>  6 worst               10          15   8.38µs   9.63µs   100564.        0B     30.2
#>  7 best                50          15    8.4µs   9.74µs    98803.        0B     29.6
#>  8 worst               50          15  10.42µs  11.72µs    82669.        0B     24.8
#>  9 best               100          15   9.02µs  10.34µs    93678.        0B     28.1
#> 10 worst              100          15  12.86µs  14.28µs    67823.        0B     20.4
#> 11 best                 3         100   8.04µs   9.37µs   102771.        0B     41.1
#> 12 worst                3         100   8.35µs   9.68µs   100159.        0B     30.1
#> 13 best                 5         100   8.16µs   9.42µs   101855.        0B     30.6
#> 14 worst                5         100    8.4µs   9.76µs    97902.        0B     29.4
#> 15 best                10         100   8.04µs   9.41µs   101140.        0B     30.4
#> 16 worst               10         100   8.76µs  10.16µs    94097.        0B     28.2
#> 17 best                50         100   8.36µs   9.77µs    97913.        0B     29.4
#> 18 worst               50         100  13.93µs  15.34µs    62803.        0B     18.8
#> 19 best               100         100      9µs  10.57µs    90507.        0B     27.2
#> 20 worst              100         100  20.88µs  22.43µs    43092.        0B     12.9
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
#>  1 best                 3          15   9.55µs   11.3µs    84581.        0B     33.8
#>  2 worst                3          15   9.94µs   11.6µs    82726.        0B     33.1
#>  3 best                 5          15   9.71µs   11.3µs    84666.        0B     33.9
#>  4 worst                5          15   9.86µs   11.3µs    85344.        0B     25.6
#>  5 best                10          15   9.62µs   10.3µs    94344.        0B     28.3
#>  6 worst               10          15   10.5µs   11.2µs    87302.        0B     34.9
#>  7 best                50          15  10.64µs   11.5µs    84349.        0B     25.3
#>  8 worst               50          15  14.47µs   15.2µs    63994.        0B     25.6
#>  9 best               100          15  12.03µs     13µs    74255.        0B     29.7
#> 10 worst              100          15  19.49µs   20.6µs    46902.        0B     18.8
#> 11 best                 3         100   9.84µs   10.8µs    89259.        0B     35.7
#> 12 worst                3         100  10.61µs   11.6µs    83446.        0B     33.4
#> 13 best                 5         100   9.75µs   10.8µs    89106.        0B     35.7
#> 14 worst                5         100  10.89µs     12µs    80111.        0B     24.0
#> 15 best                10         100   9.79µs     11µs    86509.        0B     26.0
#> 16 worst               10         100  12.13µs   13.5µs    70661.        0B     28.3
#> 17 best                50         100  11.17µs   12.3µs    77943.        0B     31.2
#> 18 worst               50         100  21.06µs   22.2µs    43548.        0B     13.1
#> 19 best               100         100  12.08µs   13.3µs    72269.        0B     36.2
#> 20 worst              100         100  32.91µs   34.2µs    28413.        0B     14.2
```
