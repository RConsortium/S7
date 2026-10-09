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
#> 1 foo_S7(x)    3.33µs   3.85µs   247737.    10.9KB     49.6
#> 2 foo_S3(x)    1.47µs   1.66µs   544616.        0B     54.5
#> 3 foo_S4(x)    1.48µs   1.74µs   554626.        0B     55.5

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
#> 1 bar_S7(x, y)   6.84µs    7.7µs   126234.        0B     50.5
#> 2 bar_S4(x, y)   3.86µs   4.33µs   226390.        0B     45.3
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
#>  1 best                 3          15   3.42µs   3.92µs   248162.        0B     49.6
#>  2 worst                3          15   3.48µs   3.97µs   245338.        0B     49.1
#>  3 best                 5          15   3.42µs   3.85µs   252521.        0B     50.5
#>  4 worst                5          15   3.56µs   3.97µs   246773.        0B     49.4
#>  5 best                10          15   3.42µs    3.8µs   256981.        0B     51.4
#>  6 worst               10          15   3.65µs   4.01µs   244084.        0B     48.8
#>  7 best                50          15   3.52µs   3.97µs   245626.        0B     49.1
#>  8 worst               50          15   4.54µs    4.9µs   200268.        0B     40.1
#>  9 best               100          15   3.72µs   4.15µs   234972.        0B     47.0
#> 10 worst              100          15   5.88µs   6.24µs   157477.        0B     31.5
#> 11 best                 3         100   3.51µs   4.01µs   240767.        0B     48.2
#> 12 worst                3         100   3.62µs   4.02µs   242082.        0B     48.4
#> 13 best                 5         100   3.46µs   3.87µs   251599.        0B     50.3
#> 14 worst                5         100   3.68µs   4.06µs   240540.        0B     48.1
#> 15 best                10         100   3.45µs   3.89µs   249566.        0B     49.9
#> 16 worst               10         100    3.9µs    4.3µs   226614.        0B     45.3
#> 17 best                50         100   3.52µs   3.94µs   247177.        0B     49.4
#> 18 worst               50         100   6.72µs   7.35µs   134025.        0B     26.8
#> 19 best               100         100   3.92µs   4.37µs   223431.        0B     22.3
#> 20 worst              100         100   10.6µs  11.29µs    87243.        0B     17.5
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
#>  1 best                 3          15      5µs   5.73µs   167660.        0B     50.3
#>  2 worst                3          15   5.09µs   5.76µs   169585.        0B     50.9
#>  3 best                 5          15   5.02µs   5.64µs   171071.        0B     51.3
#>  4 worst                5          15   5.11µs   5.77µs   169227.        0B     50.8
#>  5 best                10          15   5.05µs   5.63µs   172987.        0B     51.9
#>  6 worst               10          15    5.4µs   6.05µs   160920.        0B     48.3
#>  7 best                50          15   5.12µs   6.08µs   159148.        0B     47.8
#>  8 worst               50          15   7.46µs   8.33µs   116952.        0B     23.4
#>  9 best               100          15   5.58µs   6.37µs   153285.        0B     46.0
#> 10 worst              100          15  10.12µs  10.86µs    90372.        0B     27.1
#> 11 best                 3         100   5.18µs   6.04µs   158625.        0B     47.6
#> 12 worst                3         100   5.54µs   6.58µs   146776.        0B     44.0
#> 13 best                 5         100   5.06µs   5.87µs   164366.        0B     49.3
#> 14 worst                5         100   5.61µs   6.39µs   152419.        0B     45.7
#> 15 best                10         100   5.07µs    5.7µs   170901.        0B     51.3
#> 16 worst               10         100    6.6µs    7.2µs   135430.        0B     40.6
#> 17 best                50         100   5.41µs   6.05µs   161506.        0B     48.5
#> 18 worst               50         100  11.68µs  12.37µs    79536.        0B     23.9
#> 19 best               100         100    5.7µs   6.28µs   155052.        0B     46.5
#> 20 worst              100         100  18.02µs  19.17µs    51555.        0B     10.3
```
