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
#> 1 foo_S7(x)    7.14µs   8.82µs   106516.    10.8KB     32.0
#> 2 foo_S3(x)     2.5µs   2.94µs   310166.        0B     31.0
#> 3 foo_S4(x)    2.73µs   3.25µs   295432.        0B     29.5

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
#> 1 bar_S7(x, y)  13.01µs   15.1µs    64050.        0B     25.6
#> 2 bar_S4(x, y)   6.92µs    8.1µs   119711.        0B     23.9
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
#>  1 best                 3          15   7.47µs   9.26µs   104325.        0B     31.3
#>  2 worst                3          15   7.63µs   9.43µs   102540.        0B     30.8
#>  3 best                 5          15   7.54µs   9.43µs   100199.        0B     30.1
#>  4 worst                5          15   7.76µs   9.51µs   101797.        0B     30.5
#>  5 best                10          15   7.53µs   9.29µs   104156.        0B     31.3
#>  6 worst               10          15   7.85µs   9.57µs   100868.        0B     30.3
#>  7 best                50          15   7.98µs   9.84µs    97465.        0B     29.2
#>  8 worst               50          15   9.73µs  11.48µs    84460.        0B     25.3
#>  9 best               100          15   8.64µs   10.4µs    92693.        0B     37.1
#> 10 worst              100          15  12.08µs  13.82µs    70408.        0B     21.1
#> 11 best                 3         100   7.61µs   9.42µs   101437.        0B     40.6
#> 12 worst                3         100   7.88µs   9.77µs    98260.        0B     29.5
#> 13 best                 5         100   7.55µs   9.27µs   103333.        0B     31.0
#> 14 worst                5         100   7.98µs   9.73µs    97839.        0B     29.4
#> 15 best                10         100   7.57µs   9.31µs   102060.        0B     30.6
#> 16 worst               10         100   8.35µs  10.12µs    94372.        0B     28.3
#> 17 best                50         100   8.15µs   9.98µs    95427.        0B     28.6
#> 18 worst               50         100  13.17µs  15.06µs    63660.        0B     19.1
#> 19 best               100         100   8.69µs  10.64µs    89189.        0B     26.8
#> 20 worst              100         100   19.5µs  21.45µs    44943.        0B     13.5
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
#>  1 best                 3          15    9.4µs   11.8µs    67544.        0B     20.3
#>  2 worst                3          15   9.53µs     12µs    77747.        0B     31.1
#>  3 best                 5          15   9.59µs   11.8µs    80271.        0B     32.1
#>  4 worst                5          15   9.35µs   11.9µs    80090.        0B     32.0
#>  5 best                10          15   9.45µs   10.3µs    94285.        0B     37.7
#>  6 worst               10          15  10.12µs     11µs    88273.        0B     35.3
#>  7 best                50          15  10.58µs   11.5µs    84785.        0B     33.9
#>  8 worst               50          15  13.85µs   14.7µs    66348.        0B     26.5
#>  9 best               100          15  11.93µs   13.5µs    70358.        0B     28.2
#> 10 worst              100          15  18.19µs   19.9µs    48488.        0B     19.4
#> 11 best                 3         100   9.48µs     11µs    85858.        0B     34.4
#> 12 worst                3         100  10.39µs     12µs    79094.        0B     31.7
#> 13 best                 5         100   9.32µs     11µs    85276.        0B     34.1
#> 14 worst                5         100  10.48µs   12.3µs    75192.        0B     30.1
#> 15 best                10         100   9.45µs     11µs    85360.        0B     34.2
#> 16 worst               10         100  11.65µs   13.3µs    71097.        0B     28.5
#> 17 best                50         100  10.88µs   12.7µs    74180.        0B     29.7
#> 18 worst               50         100  20.16µs   21.9µs    43875.        0B     17.6
#> 19 best               100         100  12.12µs   13.7µs    68748.        0B     34.4
#> 20 worst              100         100  31.55µs   33.5µs    29024.        0B     14.5
```
