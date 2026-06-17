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
#> 1 foo_S7(x)    7.36µs   8.52µs   107770.    10.8KB     32.3
#> 2 foo_S3(x)    2.47µs   2.75µs   330595.        0B     33.1
#> 3 foo_S4(x)    2.74µs   3.14µs   295162.        0B     29.5

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
#> 1 bar_S7(x, y)  13.71µs  15.38µs    61202.        0B     24.5
#> 2 bar_S4(x, y)   7.28µs   8.19µs   116525.        0B     23.3
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
#>  1 best                 3          15   7.58µs   9.05µs   102056.        0B     30.6
#>  2 worst                3          15   7.97µs   9.39µs   101215.        0B     30.4
#>  3 best                 5          15   7.67µs   9.16µs   105847.        0B     31.8
#>  4 worst                5          15   7.97µs   9.38µs   102876.        0B     30.9
#>  5 best                10          15   7.73µs   9.13µs   105993.        0B     31.8
#>  6 worst               10          15   8.11µs   9.65µs   100150.        0B     30.1
#>  7 best                50          15   8.21µs    9.8µs    98680.        0B     29.6
#>  8 worst               50          15  10.12µs  11.62µs    82874.        0B     24.9
#>  9 best               100          15   8.88µs  10.44µs    92045.        0B     36.8
#> 10 worst              100          15  12.76µs  14.41µs    66590.        0B     20.0
#> 11 best                 3         100    7.7µs   9.17µs   104935.        0B     31.5
#> 12 worst                3         100   8.04µs   9.52µs   100914.        0B     40.4
#> 13 best                 5         100   7.81µs   9.34µs   101693.        0B     30.5
#> 14 worst                5         100   8.14µs    9.6µs    99076.        0B     29.7
#> 15 best                10         100   7.75µs   9.25µs   102918.        0B     30.9
#> 16 worst               10         100   8.62µs  10.11µs    94560.        0B     28.4
#> 17 best                50         100   8.29µs   9.79µs    97736.        0B     29.3
#> 18 worst               50         100  13.78µs  15.35µs    61754.        0B     18.5
#> 19 best               100         100   8.89µs  10.44µs    91595.        0B     27.5
#> 20 worst              100         100  20.36µs  22.09µs    43545.        0B     13.1
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
#>  1 best                 3          15   9.34µs   11.4µs    83618.        0B     25.1
#>  2 worst                3          15  10.03µs     12µs    79581.        0B     31.8
#>  3 best                 5          15   9.62µs   11.5µs    82972.        0B     33.2
#>  4 worst                5          15   9.83µs   11.7µs    81841.        0B     32.7
#>  5 best                10          15   9.47µs   10.2µs    95030.        0B     38.0
#>  6 worst               10          15  10.29µs     11µs    88796.        0B     26.6
#>  7 best                50          15   10.5µs   11.2µs    87137.        0B     34.9
#>  8 worst               50          15  13.97µs   14.7µs    65946.        0B     19.8
#>  9 best               100          15   11.8µs   12.9µs    74317.        0B     29.7
#> 10 worst              100          15   19.2µs   20.5µs    47031.        0B     18.8
#> 11 best                 3         100   9.65µs   10.7µs    90186.        0B     36.1
#> 12 worst                3         100  10.35µs   11.2µs    86418.        0B     34.6
#> 13 best                 5         100   9.41µs   10.4µs    83781.        0B     33.5
#> 14 worst                5         100  10.47µs   11.4µs    85448.        0B     34.2
#> 15 best                10         100   9.35µs   10.4µs    92476.        0B     37.0
#> 16 worst               10         100  11.81µs   12.7µs    76188.        0B     30.5
#> 17 best                50         100  10.82µs   11.8µs    81858.        0B     32.8
#> 18 worst               50         100   20.9µs   22.2µs    43387.        0B     17.4
#> 19 best               100         100  11.74µs   12.8µs    75258.        0B     37.6
#> 20 worst              100         100  32.21µs   33.7µs    28676.        0B     17.2
```
