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
#> 1 foo_S7(x)    4.01µs   4.71µs   193409.    10.8KB     19.3
#> 2 foo_S3(x)    1.69µs   2.01µs   437282.        0B     43.7
#> 3 foo_S4(x)    1.82µs   2.23µs   422080.        0B     42.2

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
#> 1 bar_S7(x, y)    8.1µs   8.88µs   107781.        0B     32.3
#> 2 bar_S4(x, y)   4.74µs   5.08µs   192033.        0B     38.4
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
#>  1 best                 3          15      4µs   4.44µs   216317.        0B     43.3
#>  2 worst                3          15   4.15µs   4.43µs   218648.        0B     43.7
#>  3 best                 5          15   3.98µs   4.32µs   223722.        0B     44.8
#>  4 worst                5          15    4.2µs    4.5µs   215445.        0B     43.1
#>  5 best                10          15   4.04µs   4.38µs   220692.        0B     22.1
#>  6 worst               10          15   4.39µs   4.75µs   202993.        0B     40.6
#>  7 best                50          15   4.18µs   4.47µs   216248.        0B     43.3
#>  8 worst               50          15    5.5µs   5.93µs   163518.        0B     16.4
#>  9 best               100          15   4.37µs   4.96µs   192898.        0B     19.3
#> 10 worst              100          15   7.01µs   7.69µs   126312.        0B     12.6
#> 11 best                 3         100   4.13µs   5.12µs   170863.        0B     34.2
#> 12 worst                3         100   4.41µs      5µs   190816.        0B     38.2
#> 13 best                 5         100   4.13µs   4.76µs   200476.        0B     40.1
#> 14 worst                5         100   4.45µs   4.99µs   189167.        0B     18.9
#> 15 best                10         100   4.09µs   4.65µs   205238.        0B     20.5
#> 16 worst               10         100   4.72µs   5.96µs   163519.        0B     32.7
#> 17 best                50         100   4.16µs    4.7µs   202631.        0B     20.3
#> 18 worst               50         100   8.23µs   8.87µs   108649.        0B     21.7
#> 19 best               100         100   4.46µs   5.08µs   187751.        0B     18.8
#> 20 worst              100         100  12.84µs  13.89µs    70256.        0B     14.1
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
#>  1 best                 3          15   5.65µs   6.93µs   137237.        0B     41.2
#>  2 worst                3          15   5.92µs    6.8µs   141117.        0B     28.2
#>  3 best                 5          15   5.62µs   6.28µs   152951.        0B     45.9
#>  4 worst                5          15   6.05µs   6.72µs   144122.        0B     28.8
#>  5 best                10          15    5.7µs   6.39µs   150639.        0B     30.1
#>  6 worst               10          15   6.37µs   6.98µs   137843.        0B     41.4
#>  7 best                50          15   5.96µs   6.71µs   143142.        0B     43.0
#>  8 worst               50          15   8.74µs   9.45µs   103025.        0B     20.6
#>  9 best               100          15   6.27µs   7.11µs   135061.        0B     40.5
#> 10 worst              100          15  11.63µs  12.76µs    76748.        0B     15.4
#> 11 best                 3         100   5.84µs   7.05µs   135549.        0B     40.7
#> 12 worst                3         100   6.52µs   7.38µs   130494.        0B     26.1
#> 13 best                 5         100   5.68µs   6.36µs   151995.        0B     30.4
#> 14 worst                5         100   6.54µs    7.3µs   131827.        0B     39.6
#> 15 best                10         100   5.68µs   6.36µs   151770.        0B     30.4
#> 16 worst               10         100   7.48µs   8.19µs   118301.        0B     35.5
#> 17 best                50         100   6.12µs    6.7µs   143721.        0B     28.7
#> 18 worst               50         100  13.36µs  13.81µs    71103.        0B     21.3
#> 19 best               100         100   6.34µs   6.68µs   146008.        0B     29.2
#> 20 worst              100         100  21.12µs   21.7µs    44985.        0B     13.5
```
