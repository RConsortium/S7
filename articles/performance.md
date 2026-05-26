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
#> 1 foo_S7(x)    6.68µs   8.99µs   104706.    10.8KB     20.9
#> 2 foo_S3(x)    2.28µs   2.88µs   297749.        0B      0  
#> 3 foo_S4(x)    2.53µs   3.17µs   290614.        0B     29.1

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
#> 1 bar_S7(x, y)  12.16µs  15.19µs    63549.        0B     19.1
#> 2 bar_S4(x, y)   6.56µs   7.85µs   122317.        0B     24.5
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
#>  1 best                 3          15   6.78µs   9.15µs   105600.        0B     21.1
#>  2 worst                3          15   7.01µs   9.37µs   102839.        0B     30.9
#>  3 best                 5          15   6.78µs   9.13µs   105459.        0B     31.6
#>  4 worst                5          15   7.01µs   9.39µs   102876.        0B     30.9
#>  5 best                10          15   6.83µs   9.21µs   104557.        0B     31.4
#>  6 worst               10          15    7.2µs   9.46µs   101625.        0B     20.3
#>  7 best                50          15   7.15µs   9.56µs   100667.        0B     30.2
#>  8 worst               50          15   8.63µs  10.98µs    87941.        0B     26.4
#>  9 best               100          15   7.63µs  10.23µs    93800.        0B     37.5
#> 10 worst              100          15  10.61µs  13.12µs    73935.        0B     22.2
#> 11 best                 3         100   6.84µs   9.27µs   103505.        0B     31.1
#> 12 worst                3         100   7.14µs   9.72µs    98743.        0B     29.6
#> 13 best                 5         100   6.87µs   9.28µs   103444.        0B     31.0
#> 14 worst                5         100    7.2µs   9.54µs   100165.        0B     30.1
#> 15 best                10         100   6.88µs   9.28µs   102726.        0B     30.8
#> 16 worst               10         100   7.54µs   9.88µs    96917.        0B     29.1
#> 17 best                50         100   7.16µs   9.49µs   100998.        0B     20.2
#> 18 worst               50         100   11.6µs  14.13µs    68646.        0B     20.6
#> 19 best               100         100   7.76µs  10.49µs    91335.        0B     27.4
#> 20 worst              100         100     17µs  19.81µs    49178.        0B     14.8
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
#>  1 best                 3          15   8.45µs  11.67µs    81107.        0B     32.5
#>  2 worst                3          15   8.79µs  11.55µs    82958.        0B     24.9
#>  3 best                 5          15   8.54µs  11.56µs    82412.        0B     24.7
#>  4 worst                5          15   8.85µs  11.95µs    79815.        0B     31.9
#>  5 best                10          15   8.61µs  11.39µs    84038.        0B     25.2
#>  6 worst               10          15   9.17µs  10.26µs    90361.        0B     36.2
#>  7 best                50          15   9.19µs   9.97µs    96119.        0B     38.5
#>  8 worst               50          15  11.95µs  12.73µs    75840.        0B     22.8
#>  9 best               100          15  10.15µs     11µs    86913.        0B     34.8
#> 10 worst              100          15  15.91µs   17.2µs    55876.        0B     22.4
#> 11 best                 3         100   8.52µs   10.1µs    93300.        0B     37.3
#> 12 worst                3         100   9.18µs  10.54µs    89554.        0B     26.9
#> 13 best                 5         100   8.41µs   9.84µs    95294.        0B     38.1
#> 14 worst                5         100   9.36µs  10.59µs    89438.        0B     26.8
#> 15 best                10         100   8.46µs   9.84µs    95204.        0B     38.1
#> 16 worst               10         100  10.48µs  11.85µs    80014.        0B     24.0
#> 17 best                50         100   9.37µs  10.83µs    86437.        0B     25.9
#> 18 worst               50         100  17.53µs  19.13µs    50511.        0B     20.2
#> 19 best               100         100  10.26µs  11.52µs    81212.        0B     40.6
#> 20 worst              100         100  26.93µs  28.87µs    33587.        0B     10.1
```
