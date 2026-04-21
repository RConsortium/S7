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
#> 1 foo_S7(x)    7.26µs   8.59µs   108759.    18.2KB     21.8
#> 2 foo_S3(x)     2.6µs   2.88µs   315497.        0B      0  
#> 3 foo_S4(x)    2.81µs    3.2µs   300397.        0B     30.0

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
#> 1 bar_S7(x, y)  13.41µs  15.16µs    63398.        0B     25.4
#> 2 bar_S4(x, y)   7.51µs   8.45µs   114495.        0B     22.9
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
#>  1 best                 3          15   7.15µs   8.67µs   111220.        0B     22.2
#>  2 worst                3          15   7.67µs   8.99µs   107644.        0B     32.3
#>  3 best                 5          15   7.46µs   8.81µs   109257.        0B     32.8
#>  4 worst                5          15   7.79µs   9.04µs   106444.        0B     31.9
#>  5 best                10          15   7.43µs   8.82µs   108949.        0B     21.8
#>  6 worst               10          15   7.79µs   9.16µs   104294.        0B     31.3
#>  7 best                50          15   8.03µs   9.42µs   102145.        0B     30.7
#>  8 worst               50          15   9.76µs  11.29µs    84786.        0B     25.4
#>  9 best               100          15    8.5µs  10.09µs    88853.        0B     26.7
#> 10 worst              100          15  12.22µs  13.81µs    69603.        0B     20.9
#> 11 best                 3         100   7.46µs    8.9µs   107700.        0B     32.3
#> 12 worst                3         100   7.69µs   9.07µs   105532.        0B     31.7
#> 13 best                 5         100   7.45µs   8.91µs   107193.        0B     32.2
#> 14 worst                5         100   7.75µs   9.27µs   103089.        0B     30.9
#> 15 best                10         100   7.53µs   9.02µs   104698.        0B     31.4
#> 16 worst               10         100   8.25µs   9.76µs    97431.        0B     29.2
#> 17 best                50         100   7.96µs   9.46µs   100208.        0B     20.0
#> 18 worst               50         100  13.34µs  14.81µs    64499.        0B     19.4
#> 19 best               100         100   8.59µs  10.18µs    93543.        0B     28.1
#> 20 worst              100         100  19.97µs  21.61µs    44466.        0B     13.3
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
#>  1 best                 3          15   9.19µs   10.7µs    88395.        0B     26.5
#>  2 worst                3          15   9.52µs   11.1µs    84967.        0B     34.0
#>  3 best                 5          15   9.19µs   10.8µs    87489.        0B     26.3
#>  4 worst                5          15   9.61µs   11.3µs    82834.        0B     33.1
#>  5 best                10          15   9.32µs     11µs    85503.        0B     34.2
#>  6 worst               10          15    9.8µs   11.5µs    82808.        0B     24.8
#>  7 best                50          15  10.18µs     11µs    87534.        0B     35.0
#>  8 worst               50          15  13.59µs   14.5µs    66406.        0B     19.9
#>  9 best               100          15  11.33µs   12.2µs    79473.        0B     31.8
#> 10 worst              100          15  18.21µs   19.2µs    50411.        0B     20.2
#> 11 best                 3         100   9.51µs   10.6µs    91034.        0B     27.3
#> 12 worst                3         100   10.2µs   11.4µs    84196.        0B     25.3
#> 13 best                 5         100   9.11µs   10.4µs    92260.        0B     27.7
#> 14 worst                5         100  10.21µs   11.5µs    83402.        0B     25.0
#> 15 best                10         100    9.4µs   10.6µs    91399.        0B     27.4
#> 16 worst               10         100  11.61µs     13µs    74275.        0B     22.3
#> 17 best                50         100  10.61µs   11.9µs    80645.        0B     24.2
#> 18 worst               50         100  20.16µs   21.6µs    44764.        0B     17.9
#> 19 best               100         100  11.57µs   12.9µs    74183.        0B     37.1
#> 20 worst              100         100   31.3µs   32.7µs    29592.        0B     11.8
```
