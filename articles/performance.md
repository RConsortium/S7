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
#> 1 foo_S7(x)    3.62µs   4.24µs   213407.    10.9KB     42.7
#> 2 foo_S3(x)    1.42µs   1.61µs   529053.        0B     52.9
#> 3 foo_S4(x)    1.52µs   1.74µs   536040.        0B     53.6

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
#> 1 bar_S7(x, y)   7.41µs   7.96µs   119868.        0B     48.0
#> 2 bar_S4(x, y)   4.18µs   4.53µs   213937.        0B     42.8
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
#>  1 best                 3          15   3.58µs   4.11µs   230530.        0B     46.1
#>  2 worst                3          15   3.73µs   4.27µs   221721.        0B     44.4
#>  3 best                 5          15   3.61µs   4.11µs   228349.        0B     45.7
#>  4 worst                5          15    3.8µs   4.22µs   223912.        0B     44.8
#>  5 best                10          15   3.61µs   3.96µs   242736.        0B     48.6
#>  6 worst               10          15   3.91µs   4.25µs   226545.        0B     45.3
#>  7 best                50          15   3.75µs   4.09µs   235184.        0B     47.0
#>  8 worst               50          15   5.09µs   5.47µs   177014.        0B     35.4
#>  9 best               100          15   3.91µs   4.28µs   224844.        0B     45.0
#> 10 worst              100          15   6.64µs   7.05µs   137856.        0B     27.6
#> 11 best                 3         100   3.72µs   4.14µs   230675.        0B     46.1
#> 12 worst                3         100   4.04µs   4.41µs   216906.        0B     43.4
#> 13 best                 5         100   3.74µs   4.08µs   234062.        0B     46.8
#> 14 worst                5         100   4.08µs   4.42µs   217642.        0B     43.5
#> 15 best                10         100    3.7µs   4.06µs   235848.        0B     47.2
#> 16 worst               10         100   4.38µs    4.8µs   199660.        0B     39.9
#> 17 best                50         100   3.75µs    4.2µs   226791.        0B     45.4
#> 18 worst               50         100   8.88µs    9.4µs   103158.        0B     20.6
#> 19 best               100         100   4.02µs   4.42µs   216164.        0B     43.2
#> 20 worst              100         100  15.64µs  16.34µs    59711.        0B     11.9
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
#>  1 best                 3          15   5.13µs   5.91µs   157334.        0B     47.2
#>  2 worst                3          15    5.4µs   5.85µs   162851.        0B     48.9
#>  3 best                 5          15   5.12µs   5.57µs   171033.        0B     51.3
#>  4 worst                5          15   5.46µs      6µs   159233.        0B     47.8
#>  5 best                10          15   5.19µs   5.93µs   158456.        0B     47.6
#>  6 worst               10          15   5.79µs   6.38µs   149942.        0B     30.0
#>  7 best                50          15   5.42µs   5.91µs   161183.        0B     48.4
#>  8 worst               50          15   7.91µs   8.56µs   112427.        0B     33.7
#>  9 best               100          15   5.69µs    6.2µs   154367.        0B     46.3
#> 10 worst              100          15  10.92µs  11.69µs    82756.        0B     24.8
#> 11 best                 3         100   5.39µs      6µs   158379.        0B     47.5
#> 12 worst                3         100   6.19µs   6.78µs   141229.        0B     42.4
#> 13 best                 5         100   5.21µs   5.71µs   166548.        0B     50.0
#> 14 worst                5         100   6.18µs   6.67µs   144185.        0B     43.3
#> 15 best                10         100   5.16µs   5.52µs   173933.        0B     52.2
#> 16 worst               10         100   7.38µs   7.65µs   127205.        0B     38.2
#> 17 best                50         100    5.7µs      6µs   160827.        0B     48.3
#> 18 worst               50         100  15.45µs  16.04µs    60923.        0B     18.3
#> 19 best               100         100   5.83µs   6.18µs   154989.        0B     46.5
#> 20 worst              100         100  25.37µs  26.04µs    37645.        0B     11.3
```
