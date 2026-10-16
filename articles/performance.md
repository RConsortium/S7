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
#> 1 foo_S7(x)    6.67µs   7.41µs   127750.    10.9KB     25.6
#> 2 foo_S3(x)    2.56µs   2.86µs   314152.        0B     31.4
#> 3 foo_S4(x)    2.75µs   3.08µs   312824.        0B     31.3

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
#> 1 bar_S7(x, y)  13.62µs  14.79µs    65282.        0B     26.1
#> 2 bar_S4(x, y)   7.31µs   8.27µs   117572.        0B     23.5
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
#>  1 best                 3          15   6.33µs   7.16µs   131446.        0B    26.3 
#>  2 worst                3          15    6.8µs   7.56µs   127964.        0B    25.6 
#>  3 best                 5          15   6.54µs   7.26µs   133309.        0B    26.7 
#>  4 worst                5          15   6.77µs   7.54µs   127488.        0B    25.5 
#>  5 best                10          15   6.54µs    7.3µs   131508.        0B    26.3 
#>  6 worst               10          15   7.09µs   7.86µs   123685.        0B    24.7 
#>  7 best                50          15   6.79µs   7.57µs   127536.        0B    25.5 
#>  8 worst               50          15   8.85µs   9.63µs   100531.        0B    20.1 
#>  9 best               100          15   7.19µs   7.96µs   121202.        0B    24.2 
#> 10 worst              100          15  11.24µs  12.04µs    80740.        0B    16.2 
#> 11 best                 3         100   6.73µs   7.59µs   125866.        0B    25.2 
#> 12 worst                3         100   7.18µs   7.96µs   118932.        0B    23.8 
#> 13 best                 5         100   6.74µs   7.57µs   125518.        0B    25.1 
#> 14 worst                5         100   7.21µs   8.01µs   119427.        0B    23.9 
#> 15 best                10         100   6.79µs    7.5µs   127266.        0B    25.5 
#> 16 worst               10         100   7.36µs   8.32µs   107171.        0B    21.4 
#> 17 best                50         100   6.73µs   7.53µs   126246.        0B    25.3 
#> 18 worst               50         100  12.41µs  13.23µs    72843.        0B    14.6 
#> 19 best               100         100   7.24µs   7.95µs   120830.        0B    24.2 
#> 20 worst              100         100  18.84µs  19.69µs    49117.        0B     9.83
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
#>  1 best                 3          15   9.14µs  10.14µs    94658.        0B    28.4 
#>  2 worst                3          15   9.67µs   10.8µs    87443.        0B    26.2 
#>  3 best                 5          15   9.39µs  10.38µs    90873.        0B    27.3 
#>  4 worst                5          15   9.96µs  10.92µs    86919.        0B    26.1 
#>  5 best                10          15   9.32µs  10.33µs    92266.        0B    27.7 
#>  6 worst               10          15   10.2µs  11.26µs    84111.        0B    25.2 
#>  7 best                50          15   9.76µs  10.84µs    87420.        0B    26.2 
#>  8 worst               50          15  13.62µs  14.64µs    65486.        0B    19.7 
#>  9 best               100          15  10.13µs  11.12µs    87118.        0B    17.4 
#> 10 worst              100          15  18.36µs  19.58µs    49164.        0B    14.8 
#> 11 best                 3         100   9.51µs  10.63µs    89058.        0B    26.7 
#> 12 worst                3         100  10.39µs  11.34µs    84667.        0B    25.4 
#> 13 best                 5         100   9.48µs  10.53µs    89516.        0B    26.9 
#> 14 worst                5         100  10.22µs   11.1µs    86487.        0B    26.0 
#> 15 best                10         100   9.24µs   9.88µs    98079.        0B    29.4 
#> 16 worst               10         100  11.66µs  12.32µs    78889.        0B    23.7 
#> 17 best                50         100    9.9µs  10.64µs    91233.        0B    27.4 
#> 18 worst               50         100  19.99µs  20.78µs    47001.        0B    14.1 
#> 19 best               100         100  10.26µs  10.88µs    89184.        0B    26.8 
#> 20 worst              100         100  30.69µs  31.46µs    31062.        0B     9.32
```
