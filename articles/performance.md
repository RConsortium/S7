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
#> 1 foo_S7(x)    6.39µs      7µs   134665.    10.9KB     26.9
#> 2 foo_S3(x)    2.53µs   2.75µs   331877.        0B     33.2
#> 3 foo_S4(x)    2.73µs   3.04µs   317214.        0B     31.7

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
#> 1 bar_S7(x, y)   13.3µs  14.54µs    65997.        0B     26.4
#> 2 bar_S4(x, y)   7.18µs   7.99µs   121894.        0B     24.4
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
#>  1 best                 3          15   6.31µs   7.04µs   137688.        0B     27.5
#>  2 worst                3          15   6.54µs   7.16µs   135207.        0B     27.0
#>  3 best                 5          15   6.43µs   7.01µs   138589.        0B     27.7
#>  4 worst                5          15   6.67µs   7.47µs   129807.        0B     26.0
#>  5 best                10          15   6.44µs   7.08µs   136773.        0B     27.4
#>  6 worst               10          15   6.96µs   7.64µs   127052.        0B     25.4
#>  7 best                50          15   6.64µs   7.38µs   131137.        0B     26.2
#>  8 worst               50          15   8.83µs   9.54µs   101881.        0B     20.4
#>  9 best               100          15   6.83µs   7.57µs   128224.        0B     25.6
#> 10 worst              100          15  11.03µs  11.82µs    82217.        0B     16.4
#> 11 best                 3         100   6.54µs    7.4µs   130507.        0B     26.1
#> 12 worst                3         100   6.85µs   7.65µs   125813.        0B     25.2
#> 13 best                 5         100   6.45µs   7.16µs   134806.        0B     27.0
#> 14 worst                5         100   7.02µs   7.85µs   123050.        0B     24.6
#> 15 best                10         100   6.56µs   7.28µs   132086.        0B     26.4
#> 16 worst               10         100   7.26µs   7.97µs   121442.        0B     24.3
#> 17 best                50         100    6.5µs   7.17µs   134703.        0B     26.9
#> 18 worst               50         100  11.98µs  12.79µs    75820.        0B     15.2
#> 19 best               100         100   7.05µs   7.61µs   127750.        0B     12.8
#> 20 worst              100         100  18.55µs  19.28µs    50475.        0B     10.1
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
#>  1 best                 3          15      9µs  10.01µs    96228.        0B    28.9 
#>  2 worst                3          15   9.52µs  10.39µs    93217.        0B    28.0 
#>  3 best                 5          15   9.03µs   9.88µs    97374.        0B    29.2 
#>  4 worst                5          15   9.51µs   10.4µs    92879.        0B    27.9 
#>  5 best                10          15   9.15µs  10.06µs    95974.        0B    28.8 
#>  6 worst               10          15  10.03µs  11.01µs    87566.        0B    26.3 
#>  7 best                50          15   9.47µs   10.4µs    93315.        0B    28.0 
#>  8 worst               50          15  13.21µs  14.34µs    67869.        0B    13.6 
#>  9 best               100          15   9.92µs  10.94µs    88427.        0B    26.5 
#> 10 worst              100          15  17.88µs  19.09µs    50771.        0B    15.2 
#> 11 best                 3         100   9.35µs  10.36µs    93089.        0B    27.9 
#> 12 worst                3         100  10.13µs  11.08µs    87392.        0B    26.2 
#> 13 best                 5         100   8.96µs   9.95µs    97584.        0B    29.3 
#> 14 worst                5         100  10.06µs  10.59µs    91944.        0B    27.6 
#> 15 best                10         100   9.07µs   9.58µs   101356.        0B    30.4 
#> 16 worst               10         100   11.4µs  11.92µs    81570.        0B    24.5 
#> 17 best                50         100   9.74µs  10.25µs    95039.        0B    19.0 
#> 18 worst               50         100  19.67µs  20.43µs    47689.        0B    14.3 
#> 19 best               100         100  10.08µs  10.55µs    92269.        0B    27.7 
#> 20 worst              100         100  30.51µs  31.35µs    31111.        0B     9.34
```
