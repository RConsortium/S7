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
#> 1 foo_S7(x)    3.21µs   3.87µs   244326.    10.9KB     48.9
#> 2 foo_S3(x)    1.45µs   1.72µs   527153.        0B     52.7
#> 3 foo_S4(x)    1.51µs   1.85µs   519899.        0B     52.0

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
#> 1 bar_S7(x, y)   6.74µs   7.81µs   123651.        0B     49.5
#> 2 bar_S4(x, y)   3.89µs   4.71µs   207082.        0B     20.7
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
#>  1 best                 3          15   3.23µs   4.01µs   239360.        0B     47.9
#>  2 worst                3          15   3.46µs   4.18µs   229613.        0B     45.9
#>  3 best                 5          15   3.29µs   4.04µs   238275.        0B     47.7
#>  4 worst                5          15   3.38µs   4.12µs   234387.        0B     46.9
#>  5 best                10          15   3.31µs    4.1µs   234824.        0B     47.0
#>  6 worst               10          15   3.58µs   4.36µs   222023.        0B     22.2
#>  7 best                50          15   3.52µs   4.33µs   222668.        0B     44.5
#>  8 worst               50          15   4.67µs   5.43µs   178067.        0B     35.6
#>  9 best               100          15   3.79µs   4.56µs   211487.        0B     42.3
#> 10 worst              100          15    6.1µs   6.88µs   138702.        0B     27.7
#> 11 best                 3         100   3.42µs   4.28µs   213081.        0B     42.6
#> 12 worst                3         100   3.71µs   4.51µs   212545.        0B     42.5
#> 13 best                 5         100   3.46µs   4.17µs   228214.        0B     45.7
#> 14 worst                5         100   3.54µs   4.32µs   221292.        0B     44.3
#> 15 best                10         100   3.31µs   4.06µs   235202.        0B     47.0
#> 16 worst               10         100   3.96µs   4.76µs   202335.        0B     20.2
#> 17 best                50         100   3.46µs   4.24µs   225229.        0B     45.1
#> 18 worst               50         100   6.66µs   7.46µs   129618.        0B     25.9
#> 19 best               100         100   3.67µs   4.41µs   216136.        0B     43.2
#> 20 worst              100         100  10.49µs  11.48µs    84133.        0B     16.8
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
#>  1 best                 3          15    4.6µs   5.63µs   165723.        0B     49.7
#>  2 worst                3          15   4.87µs   5.97µs   159945.        0B     32.0
#>  3 best                 5          15   4.73µs   5.74µs   165374.        0B     49.6
#>  4 worst                5          15   5.02µs   6.05µs   157137.        0B     47.2
#>  5 best                10          15    4.6µs   5.64µs   169269.        0B     50.8
#>  6 worst               10          15   5.09µs   6.15µs   154607.        0B     30.9
#>  7 best                50          15   4.87µs   5.87µs   161479.        0B     48.5
#>  8 worst               50          15   6.92µs   7.92µs   121112.        0B     24.2
#>  9 best               100          15   5.17µs   6.25µs   151542.        0B     45.5
#> 10 worst              100          15   9.38µs  10.41µs    92325.        0B     27.7
#> 11 best                 3         100   4.75µs   5.81µs   162661.        0B     32.5
#> 12 worst                3         100   5.24µs    6.3µs   149393.        0B     44.8
#> 13 best                 5         100   4.61µs   5.63µs   167193.        0B     50.2
#> 14 worst                5         100   5.28µs   6.26µs   152501.        0B     45.8
#> 15 best                10         100   4.58µs   5.45µs   175400.        0B     35.1
#> 16 worst               10         100   6.03µs   6.78µs   143586.        0B     43.1
#> 17 best                50         100   5.07µs   5.81µs   167541.        0B     33.5
#> 18 worst               50         100  10.98µs  11.81µs    82123.        0B     24.6
#> 19 best               100         100   5.27µs   6.01µs   161604.        0B     32.3
#> 20 worst              100         100  17.39µs  18.65µs    52993.        0B     15.9
```
