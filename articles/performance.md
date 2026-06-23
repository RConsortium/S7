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
#> 1 foo_S7(x)    6.72µs    8.3µs   112940.    10.8KB     33.9
#> 2 foo_S3(x)    2.31µs   2.63µs   338738.        0B     33.9
#> 3 foo_S4(x)    2.53µs   2.83µs   328830.        0B     32.9

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
#> 1 bar_S7(x, y)   12.1µs  14.28µs    67734.        0B     27.1
#> 2 bar_S4(x, y)    6.5µs   7.38µs   131323.        0B     26.3
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
#>  1 best                 3          15    6.8µs   8.49µs   113979.        0B     34.2
#>  2 worst                3          15   7.03µs   8.59µs   112412.        0B     33.7
#>  3 best                 5          15    6.9µs   8.43µs   114696.        0B     34.4
#>  4 worst                5          15    7.1µs   8.68µs   111296.        0B     33.4
#>  5 best                10          15   6.96µs    8.5µs   113869.        0B     34.2
#>  6 worst               10          15   7.24µs   8.89µs   108354.        0B     32.5
#>  7 best                50          15   7.18µs   8.94µs   107307.        0B     32.2
#>  8 worst               50          15   8.77µs  10.47µs    92540.        0B     27.8
#>  9 best               100          15   7.71µs   9.47µs   101832.        0B     30.6
#> 10 worst              100          15   10.6µs  12.43µs    78119.        0B     23.4
#> 11 best                 3         100   6.92µs   8.59µs   111970.        0B     44.8
#> 12 worst                3         100    7.2µs   8.84µs   109184.        0B     32.8
#> 13 best                 5         100   6.95µs   8.64µs   110831.        0B     33.3
#> 14 worst                5         100   7.27µs   9.04µs   106352.        0B     31.9
#> 15 best                10         100   6.91µs   8.71µs   109581.        0B     32.9
#> 16 worst               10         100   7.61µs   9.36µs   102546.        0B     30.8
#> 17 best                50         100   7.24µs   8.98µs   106846.        0B     32.1
#> 18 worst               50         100  11.66µs  13.52µs    71731.        0B     21.5
#> 19 best               100         100   7.83µs   9.67µs    99051.        0B     29.7
#> 20 worst              100         100  17.06µs  18.98µs    51408.        0B     15.4
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
#>  1 best                 3          15   8.45µs  10.64µs    89676.        0B     35.9
#>  2 worst                3          15   8.81µs  10.89µs    87848.        0B     35.2
#>  3 best                 5          15   8.49µs  10.66µs    89696.        0B     35.9
#>  4 worst                5          15    8.9µs  10.59µs    90301.        0B     27.1
#>  5 best                10          15   8.56µs   9.18µs   104681.        0B     31.4
#>  6 worst               10          15   9.29µs   9.87µs    97766.        0B     39.1
#>  7 best                50          15    9.3µs   9.88µs    98248.        0B     29.5
#>  8 worst               50          15  12.11µs  12.73µs    76198.        0B     30.5
#>  9 best               100          15  10.25µs  11.11µs    85774.        0B     34.3
#> 10 worst              100          15  15.92µs  16.95µs    57031.        0B     22.8
#> 11 best                 3         100   8.59µs   9.46µs   100673.        0B     40.3
#> 12 worst                3         100   9.28µs  10.24µs    92956.        0B     46.5
#> 13 best                 5         100   8.44µs   9.47µs    99518.        0B     39.8
#> 14 worst                5         100   9.45µs  10.47µs    90438.        0B     36.2
#> 15 best                10         100   8.53µs   9.55µs    98633.        0B     39.5
#> 16 worst               10         100  10.54µs  11.56µs    81617.        0B     24.5
#> 17 best                50         100   9.44µs  10.51µs    89507.        0B     35.8
#> 18 worst               50         100   17.6µs  18.92µs    50985.        0B     20.4
#> 19 best               100         100  10.28µs  11.53µs    81739.        0B     40.9
#> 20 worst              100         100  26.92µs  28.19µs    34431.        0B     17.2
```
