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
#> 1 foo_S7(x)    6.64µs   8.09µs   115646.    10.8KB     23.1
#> 2 foo_S3(x)    2.31µs   2.61µs   338873.        0B      0  
#> 3 foo_S4(x)    2.52µs    2.9µs   324156.        0B     32.4

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
#> 1 bar_S7(x, y)  11.81µs  13.88µs    69722.        0B     20.9
#> 2 bar_S4(x, y)   6.52µs   7.39µs   131771.        0B     26.4
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
#>  1 best                 3          15   6.68µs   8.17µs   118251.        0B     23.7
#>  2 worst                3          15   6.92µs   8.35µs   114978.        0B     34.5
#>  3 best                 5          15   6.76µs   8.23µs   117378.        0B     35.2
#>  4 worst                5          15   6.99µs   8.49µs   113319.        0B     34.0
#>  5 best                10          15   6.76µs    8.3µs   115605.        0B     34.7
#>  6 worst               10          15   7.07µs   8.66µs   111531.        0B     22.3
#>  7 best                50          15   7.09µs   8.66µs   111795.        0B     33.5
#>  8 worst               50          15   8.57µs  10.15µs    95345.        0B     28.6
#>  9 best               100          15   7.64µs   9.38µs   103150.        0B     31.0
#> 10 worst              100          15  10.51µs  12.33µs    79016.        0B     23.7
#> 11 best                 3         100   6.84µs   8.52µs   113495.        0B     34.1
#> 12 worst                3         100    7.1µs   8.78µs   110139.        0B     33.1
#> 13 best                 5         100   6.88µs   8.51µs   113712.        0B     34.1
#> 14 worst                5         100   7.19µs   8.83µs   108921.        0B     32.7
#> 15 best                10         100   6.81µs   8.45µs   113836.        0B     34.2
#> 16 worst               10         100   7.51µs   9.19µs   105249.        0B     31.6
#> 17 best                50         100   7.07µs   8.81µs   109123.        0B     32.7
#> 18 worst               50         100  11.43µs   13.3µs    73153.        0B     22.0
#> 19 best               100         100   7.68µs   9.49µs   101090.        0B     30.3
#> 20 worst              100         100  16.82µs  18.73µs    52136.        0B     15.6
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
#>  1 best                 3          15   8.34µs  10.19µs    93548.        0B     37.4
#>  2 worst                3          15   8.71µs  10.59µs    90621.        0B     27.2
#>  3 best                 5          15   8.38µs  10.21µs    93649.        0B     28.1
#>  4 worst                5          15   8.79µs  10.68µs    89622.        0B     35.9
#>  5 best                10          15    8.5µs  10.41µs    92356.        0B     27.7
#>  6 worst               10          15   9.14µs   9.91µs    95380.        0B     38.2
#>  7 best                50          15   9.14µs   9.63µs   100735.        0B     40.3
#>  8 worst               50          15  11.95µs  12.47µs    78127.        0B     23.4
#>  9 best               100          15  10.06µs  10.64µs    91270.        0B     36.5
#> 10 worst              100          15  15.79µs  16.74µs    57935.        0B     23.2
#> 11 best                 3         100   8.53µs   9.53µs   100502.        0B     30.2
#> 12 worst                3         100   9.25µs  10.19µs    93676.        0B     37.5
#> 13 best                 5         100   8.38µs   9.41µs   101916.        0B     40.8
#> 14 worst                5         100   9.26µs  10.18µs    94374.        0B     28.3
#> 15 best                10         100   8.45µs   9.44µs   100978.        0B     40.4
#> 16 worst               10         100  10.38µs  11.35µs    84609.        0B     25.4
#> 17 best                50         100   9.39µs  10.31µs    92692.        0B     27.8
#> 18 worst               50         100  17.48µs  18.63µs    52161.        0B     20.9
#> 19 best               100         100  10.14µs  10.98µs    87261.        0B     34.9
#> 20 worst              100         100  26.51µs  27.88µs    35027.        0B     14.0
```
