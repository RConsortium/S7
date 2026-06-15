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
#> 1 foo_S7(x)    6.73µs   8.17µs   115436.    10.8KB     34.6
#> 2 foo_S3(x)    2.31µs    2.6µs   343703.        0B     34.4
#> 3 foo_S4(x)    2.49µs   2.79µs   334795.        0B     33.5

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
#> 1 bar_S7(x, y)  11.98µs  13.94µs    69521.        0B     27.8
#> 2 bar_S4(x, y)   6.44µs   7.26µs   133806.        0B     26.8
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
#>  1 best                 3          15   6.74µs   8.22µs   117650.        0B     35.3
#>  2 worst                3          15   7.01µs   8.48µs   113977.        0B     34.2
#>  3 best                 5          15   6.82µs   8.33µs   115932.        0B     34.8
#>  4 worst                5          15   7.04µs   8.44µs   114957.        0B     34.5
#>  5 best                10          15   6.84µs   8.36µs   115835.        0B     34.8
#>  6 worst               10          15   7.15µs   8.68µs   111485.        0B     33.5
#>  7 best                50          15   7.21µs   8.73µs   110831.        0B     33.3
#>  8 worst               50          15   8.67µs  10.27µs    94010.        0B     28.2
#>  9 best               100          15    7.6µs   9.22µs   104442.        0B     41.8
#> 10 worst              100          15  10.57µs  12.18µs    79872.        0B     24.0
#> 11 best                 3         100   6.87µs   8.38µs   115353.        0B     34.6
#> 12 worst                3         100   7.18µs   8.67µs   111398.        0B     44.6
#> 13 best                 5         100   6.82µs   8.44µs   114219.        0B     34.3
#> 14 worst                5         100   7.25µs    8.8µs   109399.        0B     32.8
#> 15 best                10         100   6.85µs   8.38µs   114636.        0B     34.4
#> 16 worst               10         100    7.5µs   9.01µs   106765.        0B     32.0
#> 17 best                50         100   7.17µs   8.78µs   109327.        0B     32.8
#> 18 worst               50         100  11.54µs  13.32µs    72749.        0B     21.8
#> 19 best               100         100   7.71µs   9.34µs   102930.        0B     30.9
#> 20 worst              100         100  16.94µs  18.75µs    52117.        0B     15.6
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
#>  1 best                 3          15   8.38µs  10.32µs    93246.        0B     28.0
#>  2 worst                3          15   8.75µs  10.65µs    90283.        0B     36.1
#>  3 best                 5          15   8.41µs   10.3µs    92837.        0B     37.1
#>  4 worst                5          15   8.83µs  10.51µs    91803.        0B     36.7
#>  5 best                10          15   8.47µs   9.02µs   107100.        0B     42.9
#>  6 worst               10          15   9.18µs   9.71µs    99567.        0B     29.9
#>  7 best                50          15   9.21µs   9.76µs    99033.        0B     39.6
#>  8 worst               50          15  11.96µs  12.58µs    77336.        0B     23.2
#>  9 best               100          15  10.23µs  11.22µs    85443.        0B     25.6
#> 10 worst              100          15  15.89µs  17.04µs    56845.        0B     22.7
#> 11 best                 3         100   8.51µs    9.5µs   100633.        0B     30.2
#> 12 worst                3         100   9.29µs  10.24µs    93698.        0B     28.1
#> 13 best                 5         100   8.45µs   9.38µs   101671.        0B     40.7
#> 14 worst                5         100   9.32µs  10.34µs    92435.        0B     37.0
#> 15 best                10         100   8.42µs   9.43µs   100970.        0B     40.4
#> 16 worst               10         100   10.4µs  11.45µs    83978.        0B     33.6
#> 17 best                50         100   9.44µs  10.47µs    91203.        0B     36.5
#> 18 worst               50         100   17.4µs  18.54µs    52427.        0B     21.0
#> 19 best               100         100  10.19µs  11.24µs    84204.        0B     42.1
#> 20 worst              100         100  26.89µs  28.12µs    34544.        0B     17.3
```
