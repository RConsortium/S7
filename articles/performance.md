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
#> 1 foo_S7(x)    7.61µs   8.93µs   105371.    10.8KB     31.6
#> 2 foo_S3(x)    2.56µs   2.85µs   321309.        0B     32.1
#> 3 foo_S4(x)    2.81µs   3.19µs   301113.        0B     30.1

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
#> 1 bar_S7(x, y)  13.72µs  15.39µs    61977.        0B     24.8
#> 2 bar_S4(x, y)   7.21µs   8.07µs   118374.        0B     23.7
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
#>  1 best                 3          15   7.84µs   9.14µs   105823.        0B     31.8
#>  2 worst                3          15   7.95µs   9.11µs   106730.        0B     32.0
#>  3 best                 5          15   7.75µs   8.98µs   107664.        0B     32.3
#>  4 worst                5          15   8.04µs   9.19µs   105528.        0B     31.7
#>  5 best                10          15   7.71µs   9.07µs   104178.        0B     31.3
#>  6 worst               10          15   8.24µs   9.37µs   103709.        0B     31.1
#>  7 best                50          15   8.22µs   9.58µs   101188.        0B     30.4
#>  8 worst               50          15   10.3µs  11.46µs    84533.        0B     25.4
#>  9 best               100          15   8.92µs  10.22µs    94761.        0B     37.9
#> 10 worst              100          15  12.84µs  14.24µs    67818.        0B     20.4
#> 11 best                 3         100   7.83µs   9.12µs   106164.        0B     31.9
#> 12 worst                3         100   8.18µs   9.38µs   103408.        0B     41.4
#> 13 best                 5         100   7.88µs   9.15µs   105875.        0B     31.8
#> 14 worst                5         100   8.26µs   9.44µs   102321.        0B     30.7
#> 15 best                10         100   7.86µs   9.23µs   104152.        0B     31.3
#> 16 worst               10         100   8.63µs  10.15µs    93938.        0B     28.2
#> 17 best                50         100   8.29µs    9.9µs    96453.        0B     28.9
#> 18 worst               50         100  13.78µs  15.45µs    61998.        0B     18.6
#> 19 best               100         100   8.98µs  10.58µs    90062.        0B     27.0
#> 20 worst              100         100  20.82µs  22.35µs    43421.        0B     13.0
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
#>  1 best                 3          15   9.85µs   11.5µs    83821.        0B     25.2
#>  2 worst                3          15  10.13µs   11.7µs    82588.        0B     33.0
#>  3 best                 5          15   9.74µs   11.3µs    85164.        0B     34.1
#>  4 worst                5          15  10.05µs   11.6µs    83426.        0B     33.4
#>  5 best                10          15   9.75µs   10.5µs    92325.        0B     36.9
#>  6 worst               10          15  10.37µs   11.2µs    86440.        0B     25.9
#>  7 best                50          15  10.42µs   11.3µs    86216.        0B     34.5
#>  8 worst               50          15  14.21µs   14.9µs    65382.        0B     19.6
#>  9 best               100          15  11.92µs   13.2µs    73287.        0B     22.0
#> 10 worst              100          15  19.76µs     21µs    46182.        0B     18.5
#> 11 best                 3         100     10µs     11µs    87659.        0B     35.1
#> 12 worst                3         100  10.74µs   11.9µs    80868.        0B     32.4
#> 13 best                 5         100   9.81µs     11µs    87152.        0B     34.9
#> 14 worst                5         100   10.8µs   12.2µs    76952.        0B     30.8
#> 15 best                10         100   9.89µs   11.2µs    85736.        0B     34.3
#> 16 worst               10         100  12.23µs   13.6µs    70506.        0B     28.2
#> 17 best                50         100  10.98µs   12.3µs    78111.        0B     31.3
#> 18 worst               50         100  20.99µs   22.4µs    43215.        0B     17.3
#> 19 best               100         100  12.17µs   13.6µs    70553.        0B     35.3
#> 20 worst              100         100  32.82µs   34.5µs    28137.        0B     14.1
```
