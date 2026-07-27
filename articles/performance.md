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
#> 1 foo_S7(x)    8.95µs   10.3µs    91847.    10.8KB     27.6
#> 2 foo_S3(x)    2.58µs   2.88µs   316381.        0B      0  
#> 3 foo_S4(x)    2.77µs   3.19µs   300890.        0B     30.1

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
#> 1 bar_S7(x, y)  16.18µs  18.19µs    52750.        0B     26.4
#> 2 bar_S4(x, y)   7.49µs   8.41µs   115528.        0B     11.6
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
#>  1 best                 3          15   8.99µs   10.4µs    92333.        0B     27.7
#>  2 worst                3          15   9.14µs   10.7µs    86945.        0B     34.8
#>  3 best                 5          15   9.07µs   10.7µs    85279.        0B     25.6
#>  4 worst                5          15   9.29µs   10.8µs    89387.        0B     35.8
#>  5 best                10          15   9.14µs   10.6µs    91101.        0B     27.3
#>  6 worst               10          15   9.44µs     11µs    87596.        0B     35.1
#>  7 best                50          15    9.4µs     11µs    87998.        0B     35.2
#>  8 worst               50          15  11.58µs   13.1µs    73858.        0B     22.2
#>  9 best               100          15  10.05µs   11.5µs    83797.        0B     33.5
#> 10 worst              100          15   14.4µs   15.9µs    60844.        0B     18.3
#> 11 best                 3         100   9.07µs   10.6µs    90693.        0B     36.3
#> 12 worst                3         100   9.28µs   10.9µs    86479.        0B     26.0
#> 13 best                 5         100   9.13µs   10.9µs    87887.        0B     26.4
#> 14 worst                5         100   9.56µs   11.1µs    86149.        0B     25.9
#> 15 best                10         100   9.28µs   11.1µs    86372.        0B     25.9
#> 16 worst               10         100  10.12µs   11.7µs    81848.        0B     24.6
#> 17 best                50         100   9.51µs     11µs    86402.        0B     34.6
#> 18 worst               50         100  15.09µs   16.7µs    57481.        0B     17.2
#> 19 best               100         100  10.13µs   11.8µs    81373.        0B     24.4
#> 20 worst              100         100     22µs   23.7µs    40885.        0B     12.3
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
#>  1 best                 3          15   11.8µs   13.7µs    69654.        0B     34.8
#>  2 worst                3          15   11.6µs   13.7µs    70318.        0B     28.1
#>  3 best                 5          15   11.5µs   12.4µs    78135.        0B     31.3
#>  4 worst                5          15     12µs   13.1µs    74106.        0B     37.1
#>  5 best                10          15   11.7µs   12.7µs    76549.        0B     30.6
#>  6 worst               10          15   12.5µs   13.5µs    72278.        0B     36.2
#>  7 best                50          15   12.6µs   13.8µs    70228.        0B     28.1
#>  8 worst               50          15   16.4µs   17.6µs    54836.        0B     21.9
#>  9 best               100          15   13.5µs   14.8µs    65334.        0B     32.7
#> 10 worst              100          15   21.4µs   22.7µs    42693.        0B     21.4
#> 11 best                 3         100   11.8µs   13.1µs    73722.        0B     29.5
#> 12 worst                3         100   12.5µs   13.8µs    69331.        0B     34.7
#> 13 best                 5         100   11.7µs     13µs    73877.        0B     29.6
#> 14 worst                5         100   12.7µs   14.1µs    62017.        0B     31.0
#> 15 best                10         100   11.7µs     13µs    73908.        0B     37.0
#> 16 worst               10         100     14µs   15.2µs    63225.        0B     25.3
#> 17 best                50         100     13µs   14.2µs    67721.        0B     27.1
#> 18 worst               50         100     23µs   24.3µs    39842.        0B     19.9
#> 19 best               100         100     14µs   15.3µs    62739.        0B     31.4
#> 20 worst              100         100   34.3µs   35.9µs    26850.        0B     10.7
```
