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
#> 1 foo_S7(x)    3.46µs   4.04µs   235413.    10.9KB     47.1
#> 2 foo_S3(x)    1.52µs   1.76µs   515264.        0B     51.5
#> 3 foo_S4(x)    1.55µs   1.83µs   523828.        0B     52.4

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
#> 1 bar_S7(x, y)   7.23µs   8.26µs   117117.        0B     46.9
#> 2 bar_S4(x, y)   3.99µs   4.81µs   202619.        0B     40.5
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
#>  1 best                 3          15   3.52µs   4.18µs   231356.        0B     46.3
#>  2 worst                3          15   3.64µs   4.35µs   221758.        0B     44.4
#>  3 best                 5          15   3.52µs   4.25µs   226887.        0B     45.4
#>  4 worst                5          15   3.69µs   4.46µs   215376.        0B     43.1
#>  5 best                10          15   3.55µs   4.33µs   221286.        0B     44.3
#>  6 worst               10          15   3.87µs   4.69µs   205325.        0B     41.1
#>  7 best                50          15   3.72µs    4.5µs   214203.        0B     42.8
#>  8 worst               50          15   4.81µs    5.6µs   172266.        0B     34.5
#>  9 best               100          15   3.91µs    4.7µs   205515.        0B     41.1
#> 10 worst              100          15   6.08µs   6.88µs   141273.        0B     28.3
#> 11 best                 3         100   3.62µs   4.46µs   211700.        0B     42.3
#> 12 worst                3         100   3.86µs   4.63µs   205897.        0B     41.2
#> 13 best                 5         100   3.63µs   4.43µs   213447.        0B     42.7
#> 14 worst                5         100   3.89µs   4.66µs   203560.        0B     40.7
#> 15 best                10         100   3.62µs   4.23µs   228078.        0B     45.6
#> 16 worst               10         100   4.07µs   4.57µs   213472.        0B     42.7
#> 17 best                50         100   3.69µs   4.17µs   233780.        0B     46.8
#> 18 worst               50         100   7.05µs   7.43µs   132324.        0B     26.5
#> 19 best               100         100   3.96µs   4.48µs   218049.        0B     21.8
#> 20 worst              100         100  10.93µs  11.47µs    85848.        0B     17.2
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
#>  1 best                 3          15   5.03µs   5.76µs   167252.        0B     50.2
#>  2 worst                3          15   5.24µs   5.75µs   169889.        0B     51.0
#>  3 best                 5          15   5.03µs   5.62µs   173440.        0B     52.0
#>  4 worst                5          15   5.31µs   5.96µs   163477.        0B     49.1
#>  5 best                10          15   5.05µs   5.67µs   172020.        0B     51.6
#>  6 worst               10          15   5.55µs   6.16µs   158791.        0B     47.7
#>  7 best                50          15    5.3µs   6.04µs   161495.        0B     48.5
#>  8 worst               50          15   7.37µs    8.1µs   120945.        0B     24.2
#>  9 best               100          15   5.67µs   6.87µs   138024.        0B     41.4
#> 10 worst              100          15   9.81µs  11.09µs    86859.        0B     26.1
#> 11 best                 3         100   5.19µs   6.23µs   151195.        0B     45.4
#> 12 worst                3         100   5.72µs   6.68µs   144309.        0B     43.3
#> 13 best                 5         100   5.01µs   5.71µs   170335.        0B     51.1
#> 14 worst                5         100   5.78µs   6.54µs   148296.        0B     44.5
#> 15 best                10         100   5.09µs   5.88µs   164404.        0B     49.3
#> 16 worst               10         100   6.52µs   7.32µs   132867.        0B     39.9
#> 17 best                50         100   5.53µs   6.29µs   154120.        0B     46.2
#> 18 worst               50         100  11.51µs  12.33µs    79343.        0B     23.8
#> 19 best               100         100   5.69µs   6.54µs   148477.        0B     44.6
#> 20 worst              100         100   18.3µs  19.25µs    50809.        0B     10.2
```
