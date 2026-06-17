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
#> 1 foo_S7(x)    7.48µs   8.66µs   108770.    10.8KB     32.6
#> 2 foo_S3(x)    2.52µs   2.77µs   328174.        0B     32.8
#> 3 foo_S4(x)    2.71µs   3.04µs   316860.        0B     31.7

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
#> 1 bar_S7(x, y)  13.63µs     15µs    64434.        0B     25.8
#> 2 bar_S4(x, y)   7.13µs   7.83µs   124232.        0B     24.9
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
#>  1 best                 3          15    8.1µs    9.3µs   104698.        0B     31.4
#>  2 worst                3          15   8.25µs   9.42µs   103307.        0B     31.0
#>  3 best                 5          15    8.2µs   9.26µs   105016.        0B     31.5
#>  4 worst                5          15   8.48µs   9.56µs   101618.        0B     30.5
#>  5 best                10          15    8.1µs   9.36µs   104051.        0B     31.2
#>  6 worst               10          15   8.53µs   9.74µs    99720.        0B     29.9
#>  7 best                50          15   8.79µs  10.13µs    95441.        0B     28.6
#>  8 worst               50          15  10.62µs  11.89µs    81717.        0B     24.5
#>  9 best               100          15   9.46µs  10.83µs    89360.        0B     35.8
#> 10 worst              100          15  13.46µs  14.95µs    64982.        0B     19.5
#> 11 best                 3         100   8.46µs    9.7µs    99666.        0B     29.9
#> 12 worst                3         100   8.69µs   9.98µs    97415.        0B     39.0
#> 13 best                 5         100   8.27µs   9.54µs   101572.        0B     30.5
#> 14 worst                5         100   8.67µs   9.89µs    97628.        0B     29.3
#> 15 best                10         100   8.25µs    9.5µs   101564.        0B     30.5
#> 16 worst               10         100   9.11µs  10.39µs    93224.        0B     28.0
#> 17 best                50         100   8.74µs  10.14µs    95174.        0B     28.6
#> 18 worst               50         100  14.16µs  15.78µs    61394.        0B     18.4
#> 19 best               100         100   9.39µs  10.84µs    88565.        0B     26.6
#> 20 worst              100         100  21.03µs  22.66µs    42707.        0B     12.8
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
#>  1 best                 3          15  10.09µs   11.6µs    82681.        0B     33.1
#>  2 worst                3          15  10.35µs     12µs    79973.        0B     24.0
#>  3 best                 5          15   10.1µs   11.6µs    82557.        0B     24.8
#>  4 worst                5          15  10.32µs   11.9µs    81241.        0B     32.5
#>  5 best                10          15   9.87µs   10.8µs    90272.        0B     36.1
#>  6 worst               10          15  10.86µs   11.6µs    84350.        0B     33.8
#>  7 best                50          15     11µs   11.7µs    83154.        0B     33.3
#>  8 worst               50          15  14.77µs   15.5µs    62665.        0B     25.1
#>  9 best               100          15  12.16µs   13.3µs    72295.        0B     28.9
#> 10 worst              100          15  19.84µs   21.1µs    45651.        0B     18.3
#> 11 best                 3         100  10.19µs   11.4µs    83862.        0B     33.6
#> 12 worst                3         100  10.95µs   12.1µs    79148.        0B     31.7
#> 13 best                 5         100  10.02µs   11.2µs    85953.        0B     34.4
#> 14 worst                5         100  11.16µs   12.3µs    78158.        0B     23.5
#> 15 best                10         100  10.24µs   11.5µs    83177.        0B     33.3
#> 16 worst               10         100  12.48µs   13.8µs    69121.        0B     27.7
#> 17 best                50         100  11.37µs   12.7µs    75539.        0B     30.2
#> 18 worst               50         100  21.41µs   22.9µs    42075.        0B     16.8
#> 19 best               100         100  12.47µs   13.6µs    70588.        0B     35.3
#> 20 worst              100         100  33.14µs   34.4µs    28006.        0B     11.2
```
