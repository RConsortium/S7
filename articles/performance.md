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
#> 1 foo_S7(x)    6.19µs   6.91µs   136299.    10.9KB     27.3
#> 2 foo_S3(x)     2.6µs   2.87µs   318670.        0B     31.9
#> 3 foo_S4(x)    2.75µs   3.11µs   308567.        0B     30.9

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
#> 1 bar_S7(x, y)  13.11µs  14.35µs    66981.        0B     26.8
#> 2 bar_S4(x, y)   7.28µs   8.17µs   118641.        0B     11.9
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
#>  1 best                 3          15   6.28µs   7.04µs   137130.        0B     27.4
#>  2 worst                3          15   6.56µs   7.24µs   133355.        0B     26.7
#>  3 best                 5          15   6.34µs   7.01µs   137941.        0B     27.6
#>  4 worst                5          15   6.54µs   7.27µs   133169.        0B     26.6
#>  5 best                10          15   6.18µs   6.96µs   137390.        0B     27.5
#>  6 worst               10          15   6.77µs   7.53µs   128311.        0B     12.8
#>  7 best                50          15    6.5µs   7.19µs   133679.        0B     26.7
#>  8 worst               50          15   8.57µs   9.33µs   104112.        0B     20.8
#>  9 best               100          15   6.71µs   7.36µs   131313.        0B     26.3
#> 10 worst              100          15  10.84µs  11.55µs    84203.        0B     16.8
#> 11 best                 3         100   6.39µs   7.08µs   136262.        0B     27.3
#> 12 worst                3         100   6.77µs   7.47µs   129782.        0B     26.0
#> 13 best                 5         100   6.45µs   7.14µs   135093.        0B     27.0
#> 14 worst                5         100   6.89µs   7.61µs   126786.        0B     25.4
#> 15 best                10         100   6.33µs    7.1µs   134674.        0B     26.9
#> 16 worst               10         100   7.19µs   7.92µs   122218.        0B     12.2
#> 17 best                50         100   6.45µs   7.21µs   133589.        0B     26.7
#> 18 worst               50         100  11.98µs  12.76µs    76045.        0B     15.2
#> 19 best               100         100    6.9µs   7.63µs   126484.        0B     25.3
#> 20 worst              100         100  18.58µs  19.38µs    50284.        0B     10.1
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
#>  1 best                 3          15    8.7µs   9.68µs    99072.        0B    29.7 
#>  2 worst                3          15   9.08µs  10.04µs    96412.        0B    19.3 
#>  3 best                 5          15   8.64µs    9.6µs   100430.        0B    30.1 
#>  4 worst                5          15   9.23µs  10.14µs    95009.        0B    28.5 
#>  5 best                10          15    8.8µs   9.75µs    96190.        0B    28.9 
#>  6 worst               10          15   9.68µs   10.7µs    89468.        0B    17.9 
#>  7 best                50          15   9.28µs  10.34µs    91998.        0B    27.6 
#>  8 worst               50          15  13.19µs  14.33µs    67293.        0B    13.5 
#>  9 best               100          15   9.65µs  10.63µs    90255.        0B    27.1 
#> 10 worst              100          15   17.6µs  18.75µs    51394.        0B    15.4 
#> 11 best                 3         100   8.88µs   9.89µs    97515.        0B    19.5 
#> 12 worst                3         100   9.75µs   10.7µs    90808.        0B    27.3 
#> 13 best                 5         100   8.77µs   9.63µs    99729.        0B    29.9 
#> 14 worst                5         100   9.74µs  10.64µs    90599.        0B    27.2 
#> 15 best                10         100    8.5µs    9.4µs   102284.        0B    20.5 
#> 16 worst               10         100  10.91µs  11.51µs    84524.        0B    25.4 
#> 17 best                50         100   9.34µs   9.88µs    98412.        0B    19.7 
#> 18 worst               50         100   19.3µs  19.96µs    48820.        0B    14.7 
#> 19 best               100         100   9.69µs  10.26µs    94963.        0B    19.0 
#> 20 worst              100         100  30.17µs  30.91µs    31570.        0B     9.47
```
