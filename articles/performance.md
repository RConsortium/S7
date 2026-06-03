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
#> 1 foo_S7(x)    7.21µs    8.6µs   109045.    10.8KB     21.8
#> 2 foo_S3(x)    2.47µs   2.78µs   327389.        0B      0  
#> 3 foo_S4(x)    2.69µs   3.15µs   304783.        0B     30.5

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
#> 1 bar_S7(x, y)  12.96µs  14.82µs    65527.        0B     26.2
#> 2 bar_S4(x, y)   6.89µs   7.81µs   124604.        0B     24.9
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
#>  1 best                 3          15    7.4µs    8.9µs   109491.        0B     21.9
#>  2 worst                3          15   7.59µs   8.92µs   108580.        0B     32.6
#>  3 best                 5          15   7.48µs   8.84µs   109789.        0B     32.9
#>  4 worst                5          15   7.75µs   8.99µs   107936.        0B     32.4
#>  5 best                10          15    7.6µs    8.9µs   108386.        0B     21.7
#>  6 worst               10          15   7.94µs   9.39µs   103345.        0B     31.0
#>  7 best                50          15      8µs   9.45µs   102663.        0B     30.8
#>  8 worst               50          15    9.5µs  11.08µs    85710.        0B     25.7
#>  9 best               100          15   8.57µs   9.96µs    97052.        0B     29.1
#> 10 worst              100          15  11.86µs  13.22µs    73721.        0B     22.1
#> 11 best                 3         100   7.66µs   8.87µs   109107.        0B     32.7
#> 12 worst                3         100   7.96µs    9.2µs   105028.        0B     31.5
#> 13 best                 5         100   7.69µs   9.12µs   105289.        0B     31.6
#> 14 worst                5         100   8.06µs   9.41µs   102361.        0B     30.7
#> 15 best                10         100   7.71µs   9.02µs   106116.        0B     21.2
#> 16 worst               10         100   8.41µs   9.76µs    98239.        0B     29.5
#> 17 best                50         100   8.12µs   9.48µs   101617.        0B     30.5
#> 18 worst               50         100  13.25µs  14.69µs    66058.        0B     13.2
#> 19 best               100         100   8.79µs  10.28µs    93411.        0B     28.0
#> 20 worst              100         100  19.47µs   21.1µs    46018.        0B     13.8
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
#>  1 best                 3          15   9.27µs   11.2µs    85861.        0B     25.8
#>  2 worst                3          15   9.55µs   11.3µs    84251.        0B     33.7
#>  3 best                 5          15   9.32µs     11µs    87249.        0B     26.2
#>  4 worst                5          15    9.7µs   11.6µs    82316.        0B     32.9
#>  5 best                10          15   9.56µs   11.4µs    83649.        0B     25.1
#>  6 worst               10          15   9.88µs   11.4µs    83702.        0B     33.5
#>  7 best                50          15  10.29µs   11.1µs    87680.        0B     35.1
#>  8 worst               50          15  13.37µs   14.2µs    68900.        0B     20.7
#>  9 best               100          15  11.69µs   12.5µs    77989.        0B     31.2
#> 10 worst              100          15  18.15µs   19.1µs    51014.        0B     20.4
#> 11 best                 3         100    9.3µs   10.6µs    91024.        0B     27.3
#> 12 worst                3         100  10.42µs   11.6µs    83296.        0B     33.3
#> 13 best                 5         100   9.45µs   10.5µs    92142.        0B     27.7
#> 14 worst                5         100  10.43µs   11.6µs    83223.        0B     33.3
#> 15 best                10         100   9.42µs   10.7µs    90090.        0B     27.0
#> 16 worst               10         100  11.62µs   12.9µs    74635.        0B     29.9
#> 17 best                50         100  10.81µs     12µs    80237.        0B     24.1
#> 18 worst               50         100  20.17µs   21.4µs    45509.        0B     18.2
#> 19 best               100         100  12.03µs   13.1µs    74165.        0B     29.7
#> 20 worst              100         100  31.42µs   32.8µs    29860.        0B     11.9
```
