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
#> 1 foo_S7(x)    6.32µs   7.14µs   132163.    10.9KB     26.4
#> 2 foo_S3(x)    2.56µs   2.87µs   316452.        0B     31.6
#> 3 foo_S4(x)    2.71µs   3.06µs   316188.        0B     31.6

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
#> 1 bar_S7(x, y)  13.44µs  14.65µs    65825.        0B     26.3
#> 2 bar_S4(x, y)   7.31µs   8.21µs   118168.        0B     23.6
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
#>  1 best                 3          15   6.23µs   7.17µs   128789.        0B    25.8 
#>  2 worst                3          15   6.48µs   7.32µs   131282.        0B    26.3 
#>  3 best                 5          15   6.29µs    7.1µs   134392.        0B    26.9 
#>  4 worst                5          15   6.63µs   7.42µs   130590.        0B    26.1 
#>  5 best                10          15   6.31µs   7.15µs   133944.        0B    26.8 
#>  6 worst               10          15   6.77µs   7.54µs   128355.        0B    25.7 
#>  7 best                50          15   6.59µs   7.49µs   128112.        0B    25.6 
#>  8 worst               50          15   8.79µs   9.67µs   100516.        0B    20.1 
#>  9 best               100          15   6.97µs   7.88µs   121453.        0B    24.3 
#> 10 worst              100          15  11.19µs  12.14µs    79186.        0B    15.8 
#> 11 best                 3         100    6.6µs   7.48µs   128710.        0B    25.7 
#> 12 worst                3         100   6.94µs   7.72µs   124501.        0B    24.9 
#> 13 best                 5         100   6.42µs   7.26µs   131680.        0B    26.3 
#> 14 worst                5         100   6.91µs   7.67µs   124970.        0B    25.0 
#> 15 best                10         100   6.57µs   7.34µs   130718.        0B    26.1 
#> 16 worst               10         100   7.39µs   8.19µs   117608.        0B    23.5 
#> 17 best                50         100   6.45µs   7.45µs   128859.        0B    25.8 
#> 18 worst               50         100  12.98µs  13.96µs    69579.        0B    13.9 
#> 19 best               100         100   6.96µs   7.78µs   123209.        0B    24.6 
#> 20 worst              100         100   20.5µs  21.61µs    44910.        0B     8.98
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
#>  1 best                 3          15   8.86µs   9.96µs    95303.        0B    28.6 
#>  2 worst                3          15   9.28µs  10.42µs    91849.        0B    27.6 
#>  3 best                 5          15    8.9µs   9.96µs    95582.        0B    28.7 
#>  4 worst                5          15    9.5µs  10.52µs    91150.        0B    27.4 
#>  5 best                10          15   8.77µs   9.98µs    90105.        0B    27.0 
#>  6 worst               10          15   9.82µs  10.92µs    88095.        0B    17.6 
#>  7 best                50          15   9.32µs  10.48µs    90911.        0B    27.3 
#>  8 worst               50          15   13.8µs  14.96µs    64162.        0B    19.3 
#>  9 best               100          15   9.98µs  10.99µs    87357.        0B    26.2 
#> 10 worst              100          15  19.04µs  20.31µs    47289.        0B    14.2 
#> 11 best                 3         100   9.18µs  10.44µs    90881.        0B    27.3 
#> 12 worst                3         100  10.21µs  11.25µs    85450.        0B    25.6 
#> 13 best                 5         100   9.15µs   10.2µs    93628.        0B    28.1 
#> 14 worst                5         100  10.04µs  10.89µs    88409.        0B    26.5 
#> 15 best                10         100   8.87µs   9.43µs   102769.        0B    30.8 
#> 16 worst               10         100  11.56µs  12.17µs    79770.        0B    23.9 
#> 17 best                50         100   9.44µs  10.18µs    91020.        0B    27.3 
#> 18 worst               50         100  21.12µs  21.81µs    44651.        0B    13.4 
#> 19 best               100         100  10.05µs  10.76µs    90252.        0B    27.1 
#> 20 worst              100         100   33.9µs  34.73µs    28061.        0B     8.42
```
