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
#> 1 foo_S7(x)    6.37µs   7.01µs   134786.    10.9KB     27.0
#> 2 foo_S3(x)    2.58µs   2.84µs   319396.        0B     31.9
#> 3 foo_S4(x)    2.73µs   3.05µs   314864.        0B     31.5

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
#> 1 bar_S7(x, y)  13.37µs  14.43µs    66827.        0B     26.7
#> 2 bar_S4(x, y)   7.18µs   7.95µs   122291.        0B     24.5
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
#>  1 best                 3          15   6.38µs   7.05µs   137364.        0B     27.5
#>  2 worst                3          15   6.54µs   7.11µs   136626.        0B     27.3
#>  3 best                 5          15   6.31µs   6.96µs   138207.        0B     27.6
#>  4 worst                5          15   6.64µs   7.22µs   134545.        0B     26.9
#>  5 best                10          15   6.33µs   6.99µs   138341.        0B     27.7
#>  6 worst               10          15   6.85µs   7.43µs   130682.        0B     26.1
#>  7 best                50          15   6.67µs   7.29µs   132522.        0B     26.5
#>  8 worst               50          15   8.72µs   9.36µs   104023.        0B     20.8
#>  9 best               100          15   6.92µs    7.5µs   129608.        0B     25.9
#> 10 worst              100          15  10.92µs  11.72µs    82657.        0B     16.5
#> 11 best                 3         100   6.61µs   7.29µs   132710.        0B     26.5
#> 12 worst                3         100   6.96µs   7.54µs   128233.        0B     25.7
#> 13 best                 5         100   6.62µs   7.25µs   133451.        0B     26.7
#> 14 worst                5         100   6.99µs   7.59µs   126854.        0B     25.4
#> 15 best                10         100   6.58µs   7.32µs   131501.        0B     26.3
#> 16 worst               10         100    7.4µs   8.14µs   118184.        0B     23.6
#> 17 best                50         100   6.67µs   7.42µs   129662.        0B     25.9
#> 18 worst               50         100  12.17µs  12.95µs    75107.        0B     15.0
#> 19 best               100         100   6.93µs   7.62µs   126469.        0B     12.6
#> 20 worst              100         100  18.52µs  19.41µs    50181.        0B     10.0
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
#>  1 best                 3          15   8.91µs   9.97µs    95808.        0B    28.8 
#>  2 worst                3          15   9.45µs   10.3µs    93883.        0B    28.2 
#>  3 best                 5          15   9.05µs   9.86µs    97833.        0B    29.4 
#>  4 worst                5          15   9.66µs  10.44µs    92662.        0B    27.8 
#>  5 best                10          15   9.13µs  10.09µs    95632.        0B    28.7 
#>  6 worst               10          15   9.98µs  10.94µs    88347.        0B    26.5 
#>  7 best                50          15   9.28µs  10.46µs    92125.        0B    27.6 
#>  8 worst               50          15  13.31µs  14.19µs    68593.        0B    13.7 
#>  9 best               100          15   9.92µs  10.73µs    90178.        0B    27.1 
#> 10 worst              100          15  17.81µs  18.66µs    52275.        0B    15.7 
#> 11 best                 3         100   9.31µs  10.09µs    96367.        0B    28.9 
#> 12 worst                3         100  10.06µs  10.73µs    90780.        0B    27.2 
#> 13 best                 5         100   8.99µs   9.73µs    99996.        0B    30.0 
#> 14 worst                5         100  10.04µs  10.53µs    92453.        0B    27.7 
#> 15 best                10         100   9.05µs   9.51µs   102475.        0B    30.8 
#> 16 worst               10         100  11.37µs  11.79µs    82570.        0B    24.8 
#> 17 best                50         100   9.71µs  10.13µs    96402.        0B    28.9 
#> 18 worst               50         100  19.63µs  20.12µs    48644.        0B    14.6 
#> 19 best               100         100  10.06µs  10.54µs    92470.        0B    27.7 
#> 20 worst              100         100  30.61µs  31.27µs    31224.        0B     9.37
```
