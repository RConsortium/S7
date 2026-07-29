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
#> 1 foo_S7(x)    8.36µs   9.64µs    97603.    10.8KB     29.3
#> 2 foo_S3(x)    2.56µs   2.83µs   320682.        0B     32.1
#> 3 foo_S4(x)    2.75µs    3.1µs   309911.        0B     31.0

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
#> 1 bar_S7(x, y)  15.05µs  16.75µs    57658.        0B     28.8
#> 2 bar_S4(x, y)   7.24µs   8.05µs   120831.        0B     24.2
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
#>  1 best                 3          15   8.34µs    9.6µs   100725.        0B     30.2
#>  2 worst                3          15   8.51µs   9.72µs    99752.        0B     29.9
#>  3 best                 5          15   8.32µs   9.52µs   101942.        0B     30.6
#>  4 worst                5          15   8.54µs   9.67µs   100308.        0B     30.1
#>  5 best                10          15   8.32µs   9.57µs   100822.        0B     30.3
#>  6 worst               10          15   8.77µs  10.04µs    96554.        0B     29.0
#>  7 best                50          15   8.59µs    9.9µs    97703.        0B     29.3
#>  8 worst               50          15   10.6µs   11.9µs    81275.        0B     24.4
#>  9 best               100          15   8.84µs  10.08µs    95889.        0B     28.8
#> 10 worst              100          15  12.95µs  14.36µs    67085.        0B     26.8
#> 11 best                 3         100   8.44µs   9.79µs    98974.        0B     29.7
#> 12 worst                3         100   8.69µs  10.11µs    95361.        0B     38.2
#> 13 best                 5         100   8.43µs    9.8µs    98232.        0B     29.5
#> 14 worst                5         100   8.83µs  10.09µs    95470.        0B     28.6
#> 15 best                10         100    8.5µs   9.81µs    97972.        0B     29.4
#> 16 worst               10         100   9.26µs  10.69µs    89553.        0B     35.8
#> 17 best                50         100   8.58µs   9.97µs    96534.        0B     29.0
#> 18 worst               50         100  13.98µs  15.42µs    62377.        0B     25.0
#> 19 best               100         100   8.71µs   9.93µs    96691.        0B     29.0
#> 20 worst              100         100  20.23µs   21.7µs    44532.        0B     17.8
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
#>  1 best                 3          15   10.5µs   12.1µs    79117.        0B     31.7
#>  2 worst                3          15   10.8µs   12.2µs    78810.        0B     31.5
#>  3 best                 5          15   10.5µs     12µs    80637.        0B     32.3
#>  4 worst                5          15     11µs   12.1µs    79627.        0B     31.9
#>  5 best                10          15   10.5µs   11.2µs    86770.        0B     34.7
#>  6 worst               10          15   11.3µs   12.1µs    80143.        0B     32.1
#>  7 best                50          15   10.9µs   11.7µs    83193.        0B     33.3
#>  8 worst               50          15   14.7µs   15.4µs    63071.        0B     25.2
#>  9 best               100          15   11.3µs   12.3µs    78878.        0B     31.6
#> 10 worst              100          15   19.1µs   20.2µs    48041.        0B     19.2
#> 11 best                 3         100   10.8µs   11.8µs    81989.        0B     32.8
#> 12 worst                3         100   11.5µs   12.5µs    77737.        0B     31.1
#> 13 best                 5         100   10.5µs   11.5µs    84001.        0B     33.6
#> 14 worst                5         100   11.5µs   12.4µs    77515.        0B     31.0
#> 15 best                10         100   10.5µs   11.5µs    83946.        0B     33.6
#> 16 worst               10         100   12.9µs   13.9µs    69388.        0B     27.8
#> 17 best                50         100   11.2µs   12.4µs    77215.        0B     30.9
#> 18 worst               50         100   21.2µs   22.4µs    43248.        0B     17.3
#> 19 best               100         100   11.8µs   12.8µs    74630.        0B     29.9
#> 20 worst              100         100   32.2µs   33.4µs    29114.        0B     11.7
```
