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
#> 1 foo_S7(x)    4.91µs   5.91µs   153914.    10.9KB     30.8
#> 2 foo_S3(x)    1.94µs   2.33µs   370810.        0B     37.1
#> 3 foo_S4(x)    2.09µs   2.59µs   357599.        0B     35.8

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
#> 1 bar_S7(x, y)  10.04µs  11.99µs    80322.        0B     32.1
#> 2 bar_S4(x, y)   5.57µs   6.57µs   145638.        0B     29.1
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
#>  1 best                 3          15   4.92µs   6.01µs   156830.        0B     31.4
#>  2 worst                3          15   5.09µs   6.21µs   153066.        0B     30.6
#>  3 best                 5          15   4.85µs   6.01µs   156988.        0B     31.4
#>  4 worst                5          15   5.14µs   6.31µs   150338.        0B     30.1
#>  5 best                10          15   4.95µs   6.07µs   154756.        0B     31.0
#>  6 worst               10          15   5.28µs   6.44µs   147940.        0B     29.6
#>  7 best                50          15    5.1µs   6.18µs   153719.        0B     30.7
#>  8 worst               50          15    6.7µs    7.8µs   122615.        0B     24.5
#>  9 best               100          15   5.35µs   6.57µs   145077.        0B     29.0
#> 10 worst              100          15   8.37µs   9.62µs    99880.        0B     20.0
#> 11 best                 3         100   4.98µs   6.21µs   150618.        0B     30.1
#> 12 worst                3         100   5.33µs   6.41µs   147599.        0B     29.5
#> 13 best                 5         100   5.03µs   6.07µs   155063.        0B     31.0
#> 14 worst                5         100   5.39µs   6.43µs   147606.        0B     29.5
#> 15 best                10         100   5.02µs   6.11µs   154322.        0B     30.9
#> 16 worst               10         100   5.68µs   6.86µs   138229.        0B     27.7
#> 17 best                50         100    5.1µs   6.15µs   153936.        0B     30.8
#> 18 worst               50         100   9.89µs  11.06µs    87455.        0B     17.5
#> 19 best               100         100   5.42µs   6.49µs   146877.        0B     14.7
#> 20 worst              100         100  15.24µs  16.51µs    58839.        0B     11.8
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
#>  1 best                 3          15   7.05µs   8.65µs   109655.        0B     32.9
#>  2 worst                3          15   7.35µs   8.99µs   105991.        0B     31.8
#>  3 best                 5          15   7.04µs   8.62µs   110310.        0B     33.1
#>  4 worst                5          15   7.53µs   9.11µs   104677.        0B     31.4
#>  5 best                10          15   7.11µs   8.72µs   109219.        0B     32.8
#>  6 worst               10          15    7.9µs   9.49µs   100445.        0B     30.1
#>  7 best                50          15   7.38µs   9.06µs   105035.        0B     31.5
#>  8 worst               50          15  10.61µs  12.33µs    78330.        0B     15.7
#>  9 best               100          15    7.8µs   9.38µs   101350.        0B     30.4
#> 10 worst              100          15  14.13µs  15.99µs    60506.        0B     18.2
#> 11 best                 3         100   7.28µs   8.86µs   107038.        0B     32.1
#> 12 worst                3         100   8.02µs    9.6µs    99347.        0B     29.8
#> 13 best                 5         100   7.07µs   8.28µs   113553.        0B     34.1
#> 14 worst                5         100   8.02µs   8.62µs   111209.        0B     33.4
#> 15 best                10         100   7.04µs   7.64µs   125579.        0B     37.7
#> 16 worst               10         100   9.09µs   9.76µs    98853.        0B     29.7
#> 17 best                50         100   7.62µs   8.24µs   117006.        0B     23.4
#> 18 worst               50         100   15.9µs  16.61µs    58718.        0B     17.6
#> 19 best               100         100   7.85µs   8.48µs   113511.        0B     34.1
#> 20 worst              100         100  25.01µs  26.05µs    37309.        0B     11.2
```
