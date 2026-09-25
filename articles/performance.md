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
#> 1 foo_S7(x)    4.74µs   5.73µs   158800.    10.8KB     15.9
#> 2 foo_S3(x)    2.02µs   2.52µs   352635.        0B     35.3
#> 3 foo_S4(x)    2.16µs   2.75µs   340951.        0B     34.1

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
#> 1 bar_S7(x, y)   9.83µs  11.71µs    82247.        0B     24.7
#> 2 bar_S4(x, y)   5.69µs   6.76µs   138651.        0B     27.7
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
#>  1 best                 3          15   4.74µs   5.78µs   163743.        0B     32.8
#>  2 worst                3          15   4.94µs      6µs   155508.        0B     31.1
#>  3 best                 5          15   4.75µs   5.89µs   156754.        0B     31.4
#>  4 worst                5          15   5.01µs   6.05µs   154529.        0B     30.9
#>  5 best                10          15   4.82µs   5.86µs   161148.        0B     16.1
#>  6 worst               10          15   5.17µs   6.23µs   151393.        0B     30.3
#>  7 best                50          15   4.96µs   6.06µs   155730.        0B     31.2
#>  8 worst               50          15   6.59µs   7.71µs   123254.        0B     12.3
#>  9 best               100          15   5.19µs   6.25µs   152494.        0B     15.3
#> 10 worst              100          15   8.39µs    9.5µs   101329.        0B     10.1
#> 11 best                 3         100   4.87µs   5.96µs   157094.        0B     31.4
#> 12 worst                3         100    5.2µs   6.18µs   152885.        0B     30.6
#> 13 best                 5         100   4.76µs   6.05µs   155305.        0B     31.1
#> 14 worst                5         100   5.28µs   6.19µs   153065.        0B     15.3
#> 15 best                10         100   4.89µs   5.93µs   156802.        0B     15.7
#> 16 worst               10         100    5.6µs    6.7µs   139510.        0B     27.9
#> 17 best                50         100   4.99µs   6.09µs   154027.        0B     15.4
#> 18 worst               50         100    9.8µs  11.07µs    86121.        0B     17.2
#> 19 best               100         100   5.27µs   6.53µs   143588.        0B     14.4
#> 20 worst              100         100  15.29µs  16.71µs    57801.        0B     11.6
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
#>  1 best                 3          15   6.67µs   8.43µs   112908.        0B     33.9
#>  2 worst                3          15   7.07µs   8.96µs   104819.        0B     21.0
#>  3 best                 5          15   6.71µs   8.51µs   108159.        0B     32.5
#>  4 worst                5          15    7.2µs   8.92µs   106104.        0B     21.2
#>  5 best                10          15   6.79µs   8.55µs   108575.        0B     21.7
#>  6 worst               10          15   7.64µs   9.39µs    98541.        0B     29.6
#>  7 best                50          15    7.1µs   8.87µs   104154.        0B     31.3
#>  8 worst               50          15  10.45µs  12.23µs    78218.        0B     15.6
#>  9 best               100          15    7.5µs   9.26µs   100430.        0B     30.1
#> 10 worst              100          15  13.96µs  16.12µs    59515.        0B     11.9
#> 11 best                 3         100   6.85µs   8.76µs   105990.        0B     31.8
#> 12 worst                3         100   7.74µs   9.53µs    98463.        0B     19.7
#> 13 best                 5         100   6.74µs   8.64µs   108016.        0B     21.6
#> 14 worst                5         100   7.77µs   9.54µs    98569.        0B     29.6
#> 15 best                10         100   6.78µs   8.53µs   111630.        0B     22.3
#> 16 worst               10         100   8.91µs  10.78µs    87487.        0B     26.3
#> 17 best                50         100   7.33µs   8.65µs   108874.        0B     21.8
#> 18 worst               50         100  15.96µs  16.62µs    58611.        0B     17.6
#> 19 best               100         100    7.6µs   8.23µs   117134.        0B     35.2
#> 20 worst              100         100  25.36µs  26.24µs    37362.        0B     11.2
```
