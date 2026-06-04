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
#> 1 foo_S7(x)    7.48µs   8.75µs   106676.    10.8KB     21.3
#> 2 foo_S3(x)    2.48µs   2.79µs   323343.        0B      0  
#> 3 foo_S4(x)    2.69µs   3.06µs   315601.        0B     31.6

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
#> 1 bar_S7(x, y)  13.76µs  15.33µs    62873.        0B     18.9
#> 2 bar_S4(x, y)   7.23µs   8.06µs   120639.        0B     24.1
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
#>  1 best                 3          15   7.57µs   8.98µs   107652.        0B     21.5
#>  2 worst                3          15   7.79µs   9.21µs   104594.        0B     31.4
#>  3 best                 5          15   7.62µs   9.04µs   106763.        0B     32.0
#>  4 worst                5          15   7.83µs   9.27µs   103740.        0B     31.1
#>  5 best                10          15   7.61µs      9µs   107470.        0B     32.3
#>  6 worst               10          15   8.12µs   9.49µs   102206.        0B     20.4
#>  7 best                50          15   8.31µs   9.62µs   100436.        0B     30.1
#>  8 worst               50          15  10.16µs  11.66µs    82601.        0B     24.8
#>  9 best               100          15   8.86µs  10.22µs    94259.        0B     28.3
#> 10 worst              100          15  12.87µs  14.47µs    66794.        0B     20.0
#> 11 best                 3         100   7.75µs   9.21µs   104818.        0B     31.5
#> 12 worst                3         100    8.1µs   9.74µs    98813.        0B     29.7
#> 13 best                 5         100   7.82µs   9.24µs   103907.        0B     41.6
#> 14 worst                5         100   8.13µs   9.58µs   101190.        0B     30.4
#> 15 best                10         100   7.72µs   9.41µs   101806.        0B     20.4
#> 16 worst               10         100   8.48µs  10.21µs    93160.        0B     28.0
#> 17 best                50         100   8.25µs   9.89µs    96549.        0B     29.0
#> 18 worst               50         100  13.62µs  15.43µs    62535.        0B     18.8
#> 19 best               100         100    8.9µs  10.57µs    90258.        0B     27.1
#> 20 worst              100         100  20.72µs  22.45µs    42953.        0B     12.9
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
#>  1 best                 3          15    9.4µs   11.2µs    84820.        0B     33.9
#>  2 worst                3          15   9.95µs   11.8µs    80765.        0B     24.2
#>  3 best                 5          15   9.45µs   11.2µs    83478.        0B     33.4
#>  4 worst                5          15   9.97µs   11.7µs    81775.        0B     24.5
#>  5 best                10          15   9.67µs   11.5µs    82692.        0B     33.1
#>  6 worst               10          15   10.4µs   11.5µs    83017.        0B     24.9
#>  7 best                50          15  10.47µs   11.3µs    84668.        0B     25.4
#>  8 worst               50          15  14.24µs   15.1µs    63936.        0B     19.2
#>  9 best               100          15  11.62µs   12.5µs    77997.        0B     31.2
#> 10 worst              100          15  19.32µs   20.5µs    47100.        0B     18.8
#> 11 best                 3         100   9.83µs   11.2µs    84960.        0B     34.0
#> 12 worst                3         100  10.61µs     12µs    80105.        0B     24.0
#> 13 best                 5         100   9.58µs   10.9µs    86846.        0B     34.8
#> 14 worst                5         100  10.57µs   11.9µs    80487.        0B     24.2
#> 15 best                10         100   9.61µs   10.9µs    87332.        0B     34.9
#> 16 worst               10         100  11.97µs   13.3µs    72156.        0B     21.7
#> 17 best                50         100  10.97µs   12.3µs    77484.        0B     23.3
#> 18 worst               50         100  21.19µs   22.6µs    42778.        0B     17.1
#> 19 best               100         100  11.94µs   13.3µs    72035.        0B     28.8
#> 20 worst              100         100  32.69µs   34.5µs    27958.        0B     11.2
```
