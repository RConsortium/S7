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
#> 1 foo_S7(x)     6.1µs   7.06µs   134305.    10.9KB     26.9
#> 2 foo_S3(x)    2.49µs    2.9µs   315710.        0B     31.6
#> 3 foo_S4(x)    2.64µs   3.13µs   304796.        0B     30.5

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
#> 1 bar_S7(x, y)  12.64µs  14.13µs    68617.        0B     27.5
#> 2 bar_S4(x, y)   7.19µs   8.11µs   119886.        0B     24.0
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
#>  1 best                 3          15   6.08µs   7.09µs   135876.        0B    27.2 
#>  2 worst                3          15   6.29µs    7.2µs   134417.        0B    26.9 
#>  3 best                 5          15   6.18µs   7.06µs   136894.        0B    27.4 
#>  4 worst                5          15   6.44µs    7.3µs   132682.        0B    26.5 
#>  5 best                10          15    6.2µs   7.08µs   136462.        0B    27.3 
#>  6 worst               10          15   6.59µs   7.54µs   128186.        0B    25.6 
#>  7 best                50          15   6.44µs   7.41µs   130220.        0B    26.0 
#>  8 worst               50          15    8.4µs   9.34µs   104183.        0B    20.8 
#>  9 best               100          15   6.77µs   7.67µs   126582.        0B    25.3 
#> 10 worst              100          15  10.53µs  11.55µs    84668.        0B    16.9 
#> 11 best                 3         100   6.34µs   7.31µs   132182.        0B    26.4 
#> 12 worst                3         100   6.73µs   7.62µs   126278.        0B    25.3 
#> 13 best                 5         100   6.31µs   7.31µs   131286.        0B    26.3 
#> 14 worst                5         100   6.79µs   7.77µs   123343.        0B    24.7 
#> 15 best                10         100   6.36µs   7.33µs   130169.        0B    26.0 
#> 16 worst               10         100   7.25µs   8.13µs   118561.        0B    23.7 
#> 17 best                50         100   6.49µs   7.35µs   131092.        0B    26.2 
#> 18 worst               50         100  12.85µs  13.87µs    69285.        0B    13.9 
#> 19 best               100         100    6.9µs   7.78µs   123922.        0B    24.8 
#> 20 worst              100         100  20.41µs  21.51µs    45489.        0B     9.10
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
#>  1 best                 3          15   8.61µs   9.98µs    95387.        0B    28.6 
#>  2 worst                3          15   8.96µs  10.21µs    93468.        0B    28.0 
#>  3 best                 5          15   8.62µs   9.91µs    95311.        0B    28.6 
#>  4 worst                5          15   9.11µs  10.28µs    93326.        0B    28.0 
#>  5 best                10          15   8.72µs   9.96µs    96044.        0B    28.8 
#>  6 worst               10          15   9.55µs  10.83µs    88487.        0B    26.6 
#>  7 best                50          15    9.2µs   10.5µs    90891.        0B    27.3 
#>  8 worst               50          15  12.96µs  14.31µs    67473.        0B    20.2 
#>  9 best               100          15   9.59µs  10.94µs    88384.        0B    17.7 
#> 10 worst              100          15  17.46µs  18.86µs    51429.        0B    15.4 
#> 11 best                 3         100   8.84µs  10.18µs    94128.        0B    28.2 
#> 12 worst                3         100    9.9µs  11.12µs    86374.        0B    25.9 
#> 13 best                 5         100   8.71µs   9.97µs    96110.        0B    28.8 
#> 14 worst                5         100    9.9µs  10.76µs    89644.        0B    26.9 
#> 15 best                10         100   8.74µs   9.36µs   104328.        0B    31.3 
#> 16 worst               10         100  11.42µs   12.1µs    80633.        0B    24.2 
#> 17 best                50         100   9.56µs  10.15µs    96123.        0B    28.8 
#> 18 worst               50         100  21.35µs  22.07µs    44376.        0B    13.3 
#> 19 best               100         100   9.87µs  10.49µs    93250.        0B    18.7 
#> 20 worst              100         100  34.05µs  34.95µs    28123.        0B     8.44
```
