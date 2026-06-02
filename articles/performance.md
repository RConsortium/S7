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
#> 1 foo_S7(x)     7.3µs    8.6µs   108761.    10.8KB     21.8
#> 2 foo_S3(x)    2.54µs    2.9µs   316366.        0B      0  
#> 3 foo_S4(x)    2.72µs   3.23µs   296633.        0B     29.7

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
#> 1 bar_S7(x, y)  12.99µs   14.7µs    66052.        0B     26.4
#> 2 bar_S4(x, y)   7.03µs      8µs   121844.        0B     24.4
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
#>  1 best                 3          15   7.49µs   8.99µs   108118.        0B     21.6
#>  2 worst                3          15   7.72µs   9.11µs   106505.        0B     32.0
#>  3 best                 5          15   7.59µs   8.98µs   108042.        0B     32.4
#>  4 worst                5          15    7.7µs   9.12µs   106432.        0B     31.9
#>  5 best                10          15   7.59µs   8.99µs   107740.        0B     32.3
#>  6 worst               10          15   7.85µs   9.19µs   105876.        0B     21.2
#>  7 best                50          15   8.09µs   9.46µs   102752.        0B     30.8
#>  8 worst               50          15   9.77µs  11.09µs    87700.        0B     26.3
#>  9 best               100          15   8.87µs  10.23µs    94970.        0B     28.5
#> 10 worst              100          15     12µs   13.4µs    72776.        0B     21.8
#> 11 best                 3         100   7.49µs   8.85µs   108657.        0B     32.6
#> 12 worst                3         100   7.96µs   9.37µs   103468.        0B     31.0
#> 13 best                 5         100   7.51µs   8.88µs   108728.        0B     43.5
#> 14 worst                5         100   7.95µs   9.32µs   103973.        0B     20.8
#> 15 best                10         100   7.67µs   9.18µs   104310.        0B     20.9
#> 16 worst               10         100   8.35µs   9.74µs    98663.        0B     29.6
#> 17 best                50         100   8.15µs   9.49µs   101416.        0B     30.4
#> 18 worst               50         100  13.32µs  14.92µs    64821.        0B     13.0
#> 19 best               100         100   8.87µs  10.55µs    90732.        0B     27.2
#> 20 worst              100         100  19.56µs   21.4µs    45435.        0B     13.6
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
#>  1 best                 3          15   9.14µs     11µs    87378.        0B     26.2
#>  2 worst                3          15   9.52µs   11.5µs    82465.        0B     33.0
#>  3 best                 5          15   9.33µs   11.3µs    84336.        0B     25.3
#>  4 worst                5          15   9.66µs   11.9µs    80209.        0B     32.1
#>  5 best                10          15   9.55µs   11.5µs    82922.        0B     33.2
#>  6 worst               10          15  10.05µs   11.4µs    83135.        0B     24.9
#>  7 best                50          15  10.44µs   11.2µs    86852.        0B     34.8
#>  8 worst               50          15  13.68µs   14.5µs    67265.        0B     20.2
#>  9 best               100          15  11.57µs   12.5µs    78019.        0B     31.2
#> 10 worst              100          15  18.15µs   19.4µs    50106.        0B     20.1
#> 11 best                 3         100   9.46µs   10.9µs    87716.        0B     35.1
#> 12 worst                3         100  10.37µs   11.7µs    82356.        0B     24.7
#> 13 best                 5         100   9.35µs   10.8µs    88691.        0B     35.5
#> 14 worst                5         100  10.44µs   11.8µs    81481.        0B     24.5
#> 15 best                10         100   9.56µs     11µs    87493.        0B     35.0
#> 16 worst               10         100  11.72µs   13.3µs    72731.        0B     21.8
#> 17 best                50         100  10.79µs   12.3µs    78242.        0B     23.5
#> 18 worst               50         100  20.24µs   21.9µs    44454.        0B     17.8
#> 19 best               100         100  12.03µs   13.4µs    71540.        0B     35.8
#> 20 worst              100         100  31.31µs   33.2µs    29299.        0B     11.7
```
