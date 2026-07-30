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
#> 1 foo_S7(x)    7.78µs   9.45µs    99738.    10.8KB     29.9
#> 2 foo_S3(x)    2.52µs    2.9µs   314923.        0B     31.5
#> 3 foo_S4(x)    2.72µs   3.25µs   295747.        0B     29.6

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
#> 1 bar_S7(x, y)  13.83µs  16.36µs    59458.        0B     29.7
#> 2 bar_S4(x, y)   7.01µs   8.15µs   119279.        0B     23.9
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
#>  1 best                 3          15   7.84µs   9.78µs    93394.        0B     28.0
#>  2 worst                3          15    7.8µs    9.9µs    92069.        0B     27.6
#>  3 best                 5          15    7.8µs   9.71µs    99705.        0B     29.9
#>  4 worst                5          15   8.09µs   9.96µs    97022.        0B     29.1
#>  5 best                10          15    7.8µs   9.71µs    99533.        0B     29.9
#>  6 worst               10          15    8.2µs  10.06µs    96262.        0B     28.9
#>  7 best                50          15   8.03µs  10.04µs    94845.        0B     28.5
#>  8 worst               50          15   9.73µs  11.55µs    83914.        0B     25.2
#>  9 best               100          15   8.39µs  10.29µs    93956.        0B     28.2
#> 10 worst              100          15  11.66µs   13.5µs    71855.        0B     28.8
#> 11 best                 3         100   7.95µs   9.84µs    98197.        0B     29.5
#> 12 worst                3         100   8.32µs  10.16µs    95300.        0B     28.6
#> 13 best                 5         100   8.07µs   9.98µs    95739.        0B     28.7
#> 14 worst                5         100   8.34µs  10.29µs    91905.        0B     27.6
#> 15 best                10         100   8.06µs   9.94µs    96013.        0B     28.8
#> 16 worst               10         100   8.76µs  10.65µs    90017.        0B     27.0
#> 17 best                50         100    8.2µs  10.04µs    95285.        0B     28.6
#> 18 worst               50         100  13.26µs  15.14µs    63806.        0B     19.1
#> 19 best               100         100   8.52µs  10.44µs    91812.        0B     27.6
#> 20 worst              100         100  19.31µs  21.27µs    45715.        0B     13.7
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
#>  1 best                 3          15  10.15µs   12.3µs    77742.        0B     31.1
#>  2 worst                3          15  10.52µs   12.7µs    75407.        0B     30.2
#>  3 best                 5          15  10.15µs   12.5µs    76505.        0B     30.6
#>  4 worst                5          15  10.26µs     12µs    79608.        0B     31.9
#>  5 best                10          15   9.98µs     11µs    88316.        0B     35.3
#>  6 worst               10          15   10.7µs   11.6µs    83657.        0B     25.1
#>  7 best                50          15  10.45µs   11.4µs    85496.        0B     34.2
#>  8 worst               50          15   13.7µs   14.7µs    66405.        0B     26.6
#>  9 best               100          15  11.09µs   12.6µs    75225.        0B     30.1
#> 10 worst              100          15  17.66µs   19.2µs    49647.        0B     19.9
#> 11 best                 3         100  10.26µs   11.9µs    79656.        0B     31.9
#> 12 worst                3         100  11.24µs   12.7µs    73973.        0B     29.6
#> 13 best                 5         100  10.14µs   11.7µs    80650.        0B     32.3
#> 14 worst                5         100  11.14µs   12.8µs    73509.        0B     29.4
#> 15 best                10         100  10.14µs   11.7µs    80333.        0B     32.1
#> 16 worst               10         100   12.4µs     14µs    67587.        0B     27.0
#> 17 best                50         100  10.67µs   12.3µs    76191.        0B     30.5
#> 18 worst               50         100  20.16µs   21.8µs    44002.        0B     17.6
#> 19 best               100         100  11.28µs   12.8µs    72956.        0B     29.2
#> 20 worst              100         100   30.5µs   32.3µs    29913.        0B     12.0
```
