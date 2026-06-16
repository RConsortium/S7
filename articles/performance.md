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
#> 1 foo_S7(x)    7.22µs   8.86µs   106033.    10.8KB     31.8
#> 2 foo_S3(x)     2.5µs   2.93µs   310743.        0B     31.1
#> 3 foo_S4(x)    2.72µs   3.23µs   295414.        0B     29.5

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
#> 1 bar_S7(x, y)  13.09µs  15.45µs    62980.        0B     25.2
#> 2 bar_S4(x, y)   6.95µs   8.14µs   119454.        0B     23.9
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
#>  1 best                 3          15   7.44µs   9.21µs   104765.        0B     31.4
#>  2 worst                3          15   7.69µs   9.43µs   102463.        0B     30.7
#>  3 best                 5          15    7.5µs   9.27µs   103924.        0B     31.2
#>  4 worst                5          15   7.67µs   9.44µs   102423.        0B     30.7
#>  5 best                10          15   7.55µs   9.31µs   103787.        0B     31.1
#>  6 worst               10          15   7.97µs   9.79µs    98817.        0B     29.7
#>  7 best                50          15   8.16µs  10.02µs    96434.        0B     28.9
#>  8 worst               50          15   9.69µs  11.65µs    82980.        0B     24.9
#>  9 best               100          15   8.92µs  10.72µs    89661.        0B     35.9
#> 10 worst              100          15  12.19µs  14.08µs    69021.        0B     20.7
#> 11 best                 3         100   7.48µs   9.32µs   103255.        0B     31.0
#> 12 worst                3         100   7.85µs   9.69µs    99034.        0B     39.6
#> 13 best                 5         100   7.56µs   9.49µs   100843.        0B     30.3
#> 14 worst                5         100   8.04µs   9.96µs    95507.        0B     28.7
#> 15 best                10         100    7.7µs    9.6µs    99073.        0B     29.7
#> 16 worst               10         100   8.46µs  10.38µs    91292.        0B     27.4
#> 17 best                50         100   8.12µs  10.06µs    94514.        0B     28.4
#> 18 worst               50         100  13.34µs  15.37µs    62542.        0B     18.8
#> 19 best               100         100   8.63µs  10.71µs    89200.        0B     26.8
#> 20 worst              100         100  19.51µs   21.6µs    44787.        0B     13.4
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
#>  1 best                 3          15    9.4µs   11.8µs    80376.        0B     24.1
#>  2 worst                3          15   9.73µs   12.2µs    77717.        0B     31.1
#>  3 best                 5          15   9.25µs   11.7µs    80830.        0B     32.3
#>  4 worst                5          15   9.36µs   11.9µs    80103.        0B     32.1
#>  5 best                10          15   9.28µs   10.1µs    96109.        0B     38.5
#>  6 worst               10          15  10.14µs     11µs    88720.        0B     26.6
#>  7 best                50          15  10.55µs   11.4µs    85422.        0B     34.2
#>  8 worst               50          15  13.62µs   14.6µs    66446.        0B     19.9
#>  9 best               100          15  11.84µs   13.5µs    71330.        0B     28.5
#> 10 worst              100          15   18.4µs   20.1µs    48225.        0B     19.3
#> 11 best                 3         100   9.37µs   11.1µs    85742.        0B     34.3
#> 12 worst                3         100  10.35µs   11.9µs    80138.        0B     32.1
#> 13 best                 5         100   9.24µs   10.9µs    86416.        0B     34.6
#> 14 worst                5         100  10.33µs   11.9µs    79784.        0B     31.9
#> 15 best                10         100   9.46µs     11µs    86279.        0B     25.9
#> 16 worst               10         100  11.81µs   13.3µs    71855.        0B     28.8
#> 17 best                50         100  10.78µs   12.3µs    77619.        0B     31.1
#> 18 worst               50         100  20.18µs   21.9µs    44320.        0B     17.7
#> 19 best               100         100  11.88µs   13.3µs    72174.        0B     36.1
#> 20 worst              100         100  31.26µs     33µs    29504.        0B     14.8
```
