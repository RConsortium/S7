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
#> 1 foo_S7(x)    7.62µs    8.8µs   106216.    10.8KB     21.2
#> 2 foo_S3(x)    2.49µs   2.75µs   331120.        0B      0  
#> 3 foo_S4(x)    2.67µs   3.03µs   316410.        0B     31.6

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
#> 1 bar_S7(x, y)  13.71µs  15.23µs    63748.        0B     25.5
#> 2 bar_S4(x, y)   7.26µs   8.06µs   120903.        0B     24.2
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
#>  1 best                 3          15   7.54µs   8.79µs   110176.        0B     22.0
#>  2 worst                3          15   7.73µs   9.03µs   107127.        0B     32.1
#>  3 best                 5          15   7.54µs   8.89µs   108645.        0B     32.6
#>  4 worst                5          15   7.78µs   9.22µs    97275.        0B     29.2
#>  5 best                10          15    7.6µs   8.88µs   108269.        0B     32.5
#>  6 worst               10          15   7.92µs   9.27µs   103478.        0B     20.7
#>  7 best                50          15   8.16µs   9.38µs   103564.        0B     31.1
#>  8 worst               50          15  10.02µs  11.31µs    85468.        0B     25.6
#>  9 best               100          15   8.79µs  10.09µs    96132.        0B     28.8
#> 10 worst              100          15  12.86µs   14.2µs    68140.        0B     20.4
#> 11 best                 3         100    7.6µs   8.97µs   107261.        0B     32.2
#> 12 worst                3         100   8.03µs   9.43µs   102437.        0B     30.7
#> 13 best                 5         100   7.62µs   8.94µs   107948.        0B     32.4
#> 14 worst                5         100   8.04µs   9.43µs   102123.        0B     30.6
#> 15 best                10         100   7.57µs   9.03µs   105923.        0B     21.2
#> 16 worst               10         100   8.44µs   9.87µs    96852.        0B     29.1
#> 17 best                50         100   7.83µs   9.41µs   102291.        0B     30.7
#> 18 worst               50         100  13.66µs  15.03µs    64384.        0B     12.9
#> 19 best               100         100   8.77µs   10.1µs    94725.        0B     28.4
#> 20 worst              100         100  20.34µs  21.75µs    44549.        0B     13.4
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
#>  1 best                 3          15   9.31µs     11µs    87087.        0B     26.1
#>  2 worst                3          15   9.68µs   11.3µs    84220.        0B     33.7
#>  3 best                 5          15   9.39µs   10.9µs    87310.        0B     26.2
#>  4 worst                5          15    9.8µs   11.5µs    82676.        0B     33.1
#>  5 best                10          15   9.43µs     11µs    86703.        0B     26.0
#>  6 worst               10          15  10.15µs   11.3µs    84084.        0B     33.6
#>  7 best                50          15  10.43µs   11.2µs    87140.        0B     34.9
#>  8 worst               50          15  14.14µs   14.9µs    64918.        0B     19.5
#>  9 best               100          15  11.45µs   12.3µs    78719.        0B     31.5
#> 10 worst              100          15  19.14µs   20.2µs    48051.        0B     19.2
#> 11 best                 3         100   9.59µs   10.7µs    90060.        0B     27.0
#> 12 worst                3         100  10.34µs   11.5µs    83664.        0B     33.5
#> 13 best                 5         100   9.42µs   10.5µs    91981.        0B     27.6
#> 14 worst                5         100   10.4µs   11.6µs    82899.        0B     33.2
#> 15 best                10         100   9.57µs   10.6µs    91099.        0B     27.3
#> 16 worst               10         100  11.88µs     13µs    74270.        0B     29.7
#> 17 best                50         100  10.79µs   11.9µs    80797.        0B     32.3
#> 18 worst               50         100  20.82µs     22µs    44199.        0B     13.3
#> 19 best               100         100   11.8µs   12.9µs    74650.        0B     29.9
#> 20 worst              100         100  32.47µs   33.8µs    28825.        0B     11.5
```
