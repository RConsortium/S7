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
#> 1 foo_S7(x)    6.32µs   6.94µs   136701.    10.9KB     27.3
#> 2 foo_S3(x)    2.56µs   2.83µs   321351.        0B     32.1
#> 3 foo_S4(x)    2.77µs    3.1µs   311663.        0B     31.2

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
#> 1 bar_S7(x, y)  13.26µs  14.33µs    67102.        0B     20.1
#> 2 bar_S4(x, y)   7.22µs   8.08µs   119989.        0B     12.0
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
#>  1 best                 3          15   6.11µs   6.95µs   139341.        0B    27.9 
#>  2 worst                3          15    6.4µs   7.05µs   137008.        0B    27.4 
#>  3 best                 5          15   6.22µs   6.94µs   138475.        0B    27.7 
#>  4 worst                5          15   6.56µs   7.16µs   135384.        0B    27.1 
#>  5 best                10          15   6.32µs   7.01µs   136869.        0B    27.4 
#>  6 worst               10          15   6.82µs   7.41µs   130679.        0B    13.1 
#>  7 best                50          15   6.52µs   7.18µs   134208.        0B    26.8 
#>  8 worst               50          15   8.49µs   9.16µs   105718.        0B    21.1 
#>  9 best               100          15   6.73µs   7.42µs   129732.        0B    26.0 
#> 10 worst              100          15  10.67µs  11.32µs    85474.        0B    17.1 
#> 11 best                 3         100   6.42µs   7.04µs   136707.        0B    27.3 
#> 12 worst                3         100   6.75µs   7.37µs   130559.        0B    26.1 
#> 13 best                 5         100   6.46µs   7.05µs   136739.        0B    27.4 
#> 14 worst                5         100   6.83µs   7.47µs   128340.        0B    25.7 
#> 15 best                10         100   6.43µs    7.1µs   135295.        0B    27.1 
#> 16 worst               10         100   7.12µs   7.78µs   124313.        0B    12.4 
#> 17 best                50         100   6.43µs   7.11µs   134888.        0B    27.0 
#> 18 worst               50         100  12.04µs  12.66µs    76456.        0B    15.3 
#> 19 best               100         100   6.93µs    7.6µs   126501.        0B    25.3 
#> 20 worst              100         100  18.64µs  19.44µs    49952.        0B     9.99
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
#>  1 best                 3          15   8.77µs   9.78µs    97425.        0B    29.2 
#>  2 worst                3          15   9.13µs  10.08µs    95604.        0B    19.1 
#>  3 best                 5          15   8.67µs   9.69µs    98662.        0B    19.7 
#>  4 worst                5          15   9.23µs  10.13µs    94937.        0B    28.5 
#>  5 best                10          15   8.77µs   9.69µs    98847.        0B    29.7 
#>  6 worst               10          15   9.44µs  10.42µs    92220.        0B    18.4 
#>  7 best                50          15   9.03µs  10.07µs    95122.        0B    28.5 
#>  8 worst               50          15  12.82µs  13.86µs    70008.        0B    14.0 
#>  9 best               100          15   9.64µs  10.58µs    90651.        0B    18.1 
#> 10 worst              100          15  17.47µs  18.57µs    51642.        0B    15.5 
#> 11 best                 3         100   9.03µs   9.94µs    96121.        0B    28.8 
#> 12 worst                3         100   9.67µs  10.58µs    91356.        0B    18.3 
#> 13 best                 5         100   8.68µs   9.59µs    99094.        0B    19.8 
#> 14 worst                5         100   9.75µs  10.61µs    90523.        0B    27.2 
#> 15 best                10         100   8.56µs   9.37µs   102389.        0B    30.7 
#> 16 worst               10         100  10.98µs  11.45µs    84962.        0B    17.0 
#> 17 best                50         100   9.37µs   9.89µs    97607.        0B    29.3 
#> 18 worst               50         100  19.39µs  19.97µs    48796.        0B     9.76
#> 19 best               100         100   9.76µs  10.37µs    93466.        0B    28.0 
#> 20 worst              100         100  30.26µs  31.03µs    31387.        0B     6.28
```
