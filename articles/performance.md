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
#> 1 foo_S7(x)    8.46µs    9.9µs    94802.    10.8KB     28.4
#> 2 foo_S3(x)    2.56µs   2.91µs   311521.        0B     31.2
#> 3 foo_S4(x)    2.79µs   3.21µs   297066.        0B     29.7

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
#> 1 bar_S7(x, y)  15.23µs   17.2µs    55941.        0B     28.0
#> 2 bar_S4(x, y)   7.25µs   8.29µs   116623.        0B     23.3
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
#>  1 best                 3          15   8.32µs   9.91µs    96875.        0B     29.1
#>  2 worst                3          15    8.6µs  10.18µs    93336.        0B     28.0
#>  3 best                 5          15   8.49µs   9.94µs    97016.        0B     29.1
#>  4 worst                5          15   8.75µs  10.23µs    93653.        0B     28.1
#>  5 best                10          15   8.41µs   9.82µs    97934.        0B     29.4
#>  6 worst               10          15   8.84µs  10.34µs    93185.        0B     28.0
#>  7 best                50          15    8.7µs  10.28µs    93582.        0B     28.1
#>  8 worst               50          15   10.7µs  12.32µs    78398.        0B     23.5
#>  9 best               100          15   9.08µs  10.65µs    90264.        0B     27.1
#> 10 worst              100          15  13.07µs   14.7µs    65033.        0B     26.0
#> 11 best                 3         100   8.65µs  10.19µs    93258.        0B     28.0
#> 12 worst                3         100   8.75µs  10.47µs    91329.        0B     36.5
#> 13 best                 5         100   8.58µs  10.22µs    93121.        0B     27.9
#> 14 worst                5         100   9.06µs   10.7µs    89230.        0B     26.8
#> 15 best                10         100   8.58µs  10.23µs    92026.        0B     36.8
#> 16 worst               10         100   9.41µs  11.07µs    86105.        0B     25.8
#> 17 best                50         100   8.71µs  10.38µs    91434.        0B     36.6
#> 18 worst               50         100  14.18µs  15.95µs    60128.        0B     18.0
#> 19 best               100         100   9.02µs  10.64µs    88565.        0B     26.6
#> 20 worst              100         100  20.77µs  22.49µs    42739.        0B     17.1
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
#>  1 best                 3          15   10.7µs   12.6µs    74854.        0B     30.0
#>  2 worst                3          15     11µs   13.2µs    72201.        0B     28.9
#>  3 best                 5          15   10.7µs   12.7µs    74265.        0B     29.7
#>  4 worst                5          15   10.9µs   12.3µs    77572.        0B     31.0
#>  5 best                10          15   10.6µs   11.6µs    83549.        0B     33.4
#>  6 worst               10          15   11.5µs   12.5µs    77891.        0B     31.2
#>  7 best                50          15     11µs   11.9µs    81161.        0B     32.5
#>  8 worst               50          15   14.8µs   15.8µs    61508.        0B     24.6
#>  9 best               100          15   11.4µs   12.6µs    76102.        0B     30.5
#> 10 worst              100          15   19.5µs   20.9µs    46325.        0B     18.5
#> 11 best                 3         100   10.8µs   12.1µs    79137.        0B     31.7
#> 12 worst                3         100   11.7µs     13µs    73673.        0B     29.5
#> 13 best                 5         100   10.7µs     12µs    79651.        0B     31.9
#> 14 worst                5         100   11.8µs   13.1µs    72932.        0B     29.2
#> 15 best                10         100   10.7µs   12.1µs    79231.        0B     31.7
#> 16 worst               10         100   12.9µs   14.2µs    66164.        0B     26.5
#> 17 best                50         100   11.5µs   12.8µs    74258.        0B     29.7
#> 18 worst               50         100   21.6µs   23.1µs    41755.        0B     16.7
#> 19 best               100         100   11.9µs   13.3µs    71801.        0B     28.7
#> 20 worst              100         100   32.4µs     34µs    28456.        0B     11.4
```
