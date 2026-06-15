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
#> 1 foo_S7(x)     6.8µs    8.3µs   113809.    10.8KB     34.2
#> 2 foo_S3(x)    2.31µs   2.59µs   341568.        0B     34.2
#> 3 foo_S4(x)    2.53µs   2.87µs   325648.        0B     32.6

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
#> 1 bar_S7(x, y)  12.07µs  14.16µs    68435.        0B     27.4
#> 2 bar_S4(x, y)   6.44µs   7.34µs   131617.        0B     26.3
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
#>  1 best                 3          15   6.92µs   8.55µs   113081.        0B     33.9
#>  2 worst                3          15   7.09µs   8.73µs   111207.        0B     33.4
#>  3 best                 5          15   6.89µs   8.48µs   113993.        0B     34.2
#>  4 worst                5          15   7.06µs   8.69µs   111502.        0B     33.5
#>  5 best                10          15   6.97µs   8.53µs   113266.        0B     34.0
#>  6 worst               10          15   7.27µs   8.84µs   109240.        0B     32.8
#>  7 best                50          15   7.34µs      9µs   107351.        0B     32.2
#>  8 worst               50          15   8.79µs  10.46µs    92615.        0B     27.8
#>  9 best               100          15   7.68µs   9.46µs   101046.        0B     40.4
#> 10 worst              100          15  10.72µs  12.51µs    77668.        0B     23.3
#> 11 best                 3         100      7µs   8.64µs   111172.        0B     44.5
#> 12 worst                3         100   7.35µs   8.96µs   107799.        0B     32.3
#> 13 best                 5         100   6.92µs   8.68µs   110232.        0B     33.1
#> 14 worst                5         100   7.28µs   8.98µs   106769.        0B     32.0
#> 15 best                10         100   7.02µs   8.69µs   110315.        0B     33.1
#> 16 worst               10         100   7.65µs   9.32µs   103098.        0B     30.9
#> 17 best                50         100    7.3µs   8.98µs   106789.        0B     32.0
#> 18 worst               50         100  11.69µs  13.56µs    71575.        0B     21.5
#> 19 best               100         100   7.77µs   9.79µs    97865.        0B     29.4
#> 20 worst              100         100  17.11µs  19.14µs    50810.        0B     15.2
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
#>  1 best                 3          15   8.58µs  11.02µs    83141.        0B     24.9
#>  2 worst                3          15   8.99µs  11.46µs    80426.        0B     32.2
#>  3 best                 5          15   8.69µs     11µs    86686.        0B     34.7
#>  4 worst                5          15      9µs  11.14µs    86153.        0B     34.5
#>  5 best                10          15   8.67µs   9.26µs   104104.        0B     41.7
#>  6 worst               10          15   9.36µs   9.92µs    96986.        0B     38.8
#>  7 best                50          15   9.37µs   9.96µs    97363.        0B     39.0
#>  8 worst               50          15  12.26µs  12.86µs    75165.        0B     30.1
#>  9 best               100          15  10.27µs  11.34µs    83866.        0B     33.6
#> 10 worst              100          15  15.88µs  17.07µs    56498.        0B     22.6
#> 11 best                 3         100   8.69µs   9.74µs    97095.        0B     38.9
#> 12 worst                3         100   9.47µs  10.51µs    90554.        0B     36.2
#> 13 best                 5         100    8.5µs    9.6µs    98503.        0B     39.4
#> 14 worst                5         100   9.44µs  10.62µs    88923.        0B     35.6
#> 15 best                10         100   8.69µs   9.78µs    96546.        0B     38.6
#> 16 worst               10         100  10.61µs  11.79µs    80412.        0B     32.2
#> 17 best                50         100   9.48µs  10.66µs    88659.        0B     35.5
#> 18 worst               50         100  17.55µs  18.85µs    51118.        0B     20.5
#> 19 best               100         100  10.42µs  11.66µs    80346.        0B     40.2
#> 20 worst              100         100  26.96µs  28.35µs    34287.        0B     17.2
```
