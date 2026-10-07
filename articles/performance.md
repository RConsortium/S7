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
#> 1 foo_S7(x)     6.3µs   7.16µs   131742.    10.9KB     26.4
#> 2 foo_S3(x)     2.6µs   2.92µs   311876.        0B     31.2
#> 3 foo_S4(x)    2.77µs   3.17µs   302254.        0B     30.2

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
#> 1 bar_S7(x, y)  13.16µs   14.5µs    66543.        0B     26.6
#> 2 bar_S4(x, y)   7.42µs    8.5µs   113618.        0B     11.4
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
#>  1 best                 3          15   6.41µs   7.34µs   130948.        0B    26.2 
#>  2 worst                3          15   6.49µs   7.46µs   128052.        0B    25.6 
#>  3 best                 5          15    6.3µs   7.22µs   132969.        0B    26.6 
#>  4 worst                5          15    6.6µs   7.49µs   128022.        0B    25.6 
#>  5 best                10          15    6.3µs    7.2µs   133589.        0B    26.7 
#>  6 worst               10          15   6.78µs   7.63µs   126394.        0B    12.6 
#>  7 best                50          15   6.68µs   7.57µs   126852.        0B    25.4 
#>  8 worst               50          15    8.6µs    9.6µs    99703.        0B    19.9 
#>  9 best               100          15   6.96µs   7.92µs   120633.        0B    24.1 
#> 10 worst              100          15   10.9µs   11.9µs    80830.        0B    16.2 
#> 11 best                 3         100    6.5µs   7.43µs   128231.        0B    25.7 
#> 12 worst                3         100   6.88µs   7.87µs   121149.        0B    24.2 
#> 13 best                 5         100    6.6µs   7.64µs   121492.        0B    24.3 
#> 14 worst                5         100   6.92µs   7.84µs   121621.        0B    24.3 
#> 15 best                10         100   6.57µs   7.45µs   127604.        0B    25.5 
#> 16 worst               10         100   7.37µs   8.29µs   114767.        0B    11.5 
#> 17 best                50         100   6.69µs   7.61µs   125351.        0B    25.1 
#> 18 worst               50         100  12.06µs  13.07µs    73596.        0B    14.7 
#> 19 best               100         100   6.97µs   7.97µs   119001.        0B    23.8 
#> 20 worst              100         100  18.43µs  19.58µs    49046.        0B     9.81
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
#>  1 best                 3          15   8.64µs   9.98µs    94337.        0B    28.3 
#>  2 worst                3          15   9.22µs  10.43µs    90727.        0B    18.1 
#>  3 best                 5          15   8.83µs  10.12µs    93170.        0B    28.0 
#>  4 worst                5          15    9.4µs  10.64µs    88748.        0B    26.6 
#>  5 best                10          15   8.88µs  10.12µs    93464.        0B    28.0 
#>  6 worst               10          15   9.84µs  11.07µs    85717.        0B    17.1 
#>  7 best                50          15    9.3µs   10.5µs    89639.        0B    26.9 
#>  8 worst               50          15  13.09µs  14.49µs    65806.        0B    13.2 
#>  9 best               100          15   9.89µs   11.1µs    85298.        0B    25.6 
#> 10 worst              100          15  17.88µs  19.11µs    50268.        0B    15.1 
#> 11 best                 3         100    9.1µs  10.37µs    92096.        0B    18.4 
#> 12 worst                3         100  10.03µs  11.15µs    84932.        0B    25.5 
#> 13 best                 5         100   8.81µs   10.1µs    93202.        0B    28.0 
#> 14 worst                5         100    9.8µs  11.14µs    84565.        0B    25.4 
#> 15 best                10         100   8.62µs   9.86µs    97367.        0B    19.5 
#> 16 worst               10         100  10.76µs   11.6µs    81638.        0B    24.5 
#> 17 best                50         100    9.5µs  10.24µs    94428.        0B    18.9 
#> 18 worst               50         100  19.37µs  20.19µs    48159.        0B    14.5 
#> 19 best               100         100   9.71µs  10.44µs    92403.        0B    18.5 
#> 20 worst              100         100  30.35µs  31.19µs    31226.        0B     9.37
```
