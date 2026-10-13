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
#> 1 foo_S7(x)    6.59µs   7.32µs   127382.    10.9KB     25.5
#> 2 foo_S3(x)    2.63µs   2.92µs   313546.        0B     31.4
#> 3 foo_S4(x)    2.75µs   3.12µs   308377.        0B     30.8

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
#> 1 bar_S7(x, y)  13.48µs  14.64µs    65369.        0B     26.2
#> 2 bar_S4(x, y)   7.19µs   8.12µs   119368.        0B     23.9
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
#>  1 best                 3          15   6.56µs   7.31µs   131637.        0B    26.3 
#>  2 worst                3          15   6.79µs   7.56µs   127874.        0B    25.6 
#>  3 best                 5          15   6.49µs   7.26µs   132481.        0B    26.5 
#>  4 worst                5          15   6.85µs   7.64µs   125959.        0B    25.2 
#>  5 best                10          15   6.62µs   7.38µs   130632.        0B    26.1 
#>  6 worst               10          15   7.06µs   7.95µs   121425.        0B    24.3 
#>  7 best                50          15   6.78µs   7.61µs   127120.        0B    25.4 
#>  8 worst               50          15   8.88µs   9.69µs    99810.        0B    20.0 
#>  9 best               100          15   7.02µs   7.89µs   122089.        0B    24.4 
#> 10 worst              100          15  11.16µs  12.02µs    80624.        0B    16.1 
#> 11 best                 3         100   6.66µs   7.54µs   126259.        0B    25.3 
#> 12 worst                3         100   7.02µs   7.95µs   119151.        0B    23.8 
#> 13 best                 5         100   6.85µs   7.66µs   123972.        0B    24.8 
#> 14 worst                5         100   7.17µs   7.96µs   119322.        0B    23.9 
#> 15 best                10         100   6.75µs   7.56µs   126523.        0B    25.3 
#> 16 worst               10         100   7.45µs   8.37µs   112752.        0B    22.6 
#> 17 best                50         100   6.84µs   7.66µs   124644.        0B    24.9 
#> 18 worst               50         100  12.42µs  13.27µs    72709.        0B    14.5 
#> 19 best               100         100   7.18µs      8µs   119426.        0B    23.9 
#> 20 worst              100         100  18.77µs  19.64µs    49059.        0B     9.81
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
#>  1 best                 3          15   9.26µs  10.41µs    90437.        0B    27.1 
#>  2 worst                3          15   9.56µs  10.76µs    87720.        0B    26.3 
#>  3 best                 5          15    9.3µs  10.44µs    90569.        0B    27.2 
#>  4 worst                5          15   9.81µs  10.97µs    85918.        0B    25.8 
#>  5 best                10          15   9.44µs  10.58µs    89008.        0B    26.7 
#>  6 worst               10          15  10.26µs  11.42µs    82757.        0B    24.8 
#>  7 best                50          15   9.66µs  10.88µs    86324.        0B    25.9 
#>  8 worst               50          15  13.76µs  14.86µs    64089.        0B    19.2 
#>  9 best               100          15  10.23µs  11.39µs    84094.        0B    16.8 
#> 10 worst              100          15  18.25µs  19.41µs    49309.        0B    14.8 
#> 11 best                 3         100   9.55µs  10.68µs    88558.        0B    26.6 
#> 12 worst                3         100  10.43µs  11.57µs    82515.        0B    24.8 
#> 13 best                 5         100   9.54µs  10.57µs    89749.        0B    26.9 
#> 14 worst                5         100  10.32µs  11.19µs    85663.        0B    25.7 
#> 15 best                10         100   9.16µs   9.88µs    97913.        0B    29.4 
#> 16 worst               10         100  11.61µs  12.28µs    79053.        0B    23.7 
#> 17 best                50         100   9.82µs  10.45µs    92601.        0B    27.8 
#> 18 worst               50         100  19.83µs  20.59µs    47392.        0B    14.2 
#> 19 best               100         100  10.18µs  10.83µs    90011.        0B    27.0 
#> 20 worst              100         100  30.42µs  31.34µs    31178.        0B     9.36
```
