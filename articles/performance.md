# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text <- new_class("Text", parent = class_character)
Number <- new_class("Number", parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 <- new_generic("foo_S7", "x")
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
#> 1 foo_S7(x)    7.28µs   8.75µs   106758.    10.8KB     21.4
#> 2 foo_S3(x)    2.49µs   2.79µs   327459.        0B      0  
#> 3 foo_S4(x)    2.71µs   3.19µs   302125.        0B     30.2

bar_S7 <- new_generic("bar_S7", c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)  12.97µs  14.87µs    65360.        0B     19.6
#> 2 bar_S4(x, y)   6.89µs   7.81µs   124975.        0B     25.0
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
    Text <- new_class("Text", parent = class_character)
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
    foo_S7 <- new_generic("foo_S7", "x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 <- new_generic("foo2_S7", "x")
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
#>  1 best                 3          15   7.38µs   8.87µs   109469.        0B     21.9
#>  2 worst                3          15   7.58µs   8.92µs   108756.        0B     32.6
#>  3 best                 5          15   7.42µs   8.73µs   110381.        0B     33.1
#>  4 worst                5          15   7.71µs   8.98µs   108038.        0B     32.4
#>  5 best                10          15   7.24µs   8.56µs   113476.        0B     34.1
#>  6 worst               10          15   7.75µs   9.11µs   106318.        0B     21.3
#>  7 best                50          15   8.12µs   9.42µs   103272.        0B     31.0
#>  8 worst               50          15   9.38µs  10.78µs    90347.        0B     27.1
#>  9 best               100          15   8.63µs   9.96µs    97561.        0B     29.3
#> 10 worst              100          15  12.07µs  13.35µs    72827.        0B     21.9
#> 11 best                 3         100   7.28µs   8.65µs   112133.        0B     33.7
#> 12 worst                3         100   7.84µs   9.03µs   107519.        0B     32.3
#> 13 best                 5         100    7.3µs   8.64µs   112123.        0B     33.6
#> 14 worst                5         100   7.76µs   8.97µs   107847.        0B     32.4
#> 15 best                10         100   7.43µs   8.81µs   108935.        0B     32.7
#> 16 worst               10         100   8.22µs   9.56µs   100239.        0B     30.1
#> 17 best                50         100   8.09µs    9.4µs   102459.        0B     20.5
#> 18 worst               50         100  13.26µs  14.75µs    65642.        0B     19.7
#> 19 best               100         100    8.9µs  10.36µs    92866.        0B     27.9
#> 20 worst              100         100   19.5µs  21.12µs    45790.        0B     13.7
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text <- new_class("Text", parent = class_character)
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
    foo_S7 <- new_generic("foo_S7", c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 <- new_generic("foo2_S7", c("x", "y"))
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
#>  1 best                 3          15   9.18µs   11.1µs    79141.        0B     23.7
#>  2 worst                3          15   9.15µs   11.3µs    76999.        0B     23.1
#>  3 best                 5          15    9.2µs   10.9µs    86559.        0B     26.0
#>  4 worst                5          15   9.59µs   11.3µs    84361.        0B     33.8
#>  5 best                10          15   9.38µs   11.2µs    84794.        0B     25.4
#>  6 worst               10          15   9.99µs   11.3µs    84223.        0B     33.7
#>  7 best                50          15  10.33µs   11.1µs    86902.        0B     26.1
#>  8 worst               50          15  13.49µs   14.3µs    68183.        0B     27.3
#>  9 best               100          15  11.68µs   12.4µs    78800.        0B     31.5
#> 10 worst              100          15  18.17µs   19.1µs    51059.        0B     20.4
#> 11 best                 3         100   9.55µs   10.6µs    90309.        0B     27.1
#> 12 worst                3         100  10.12µs   11.5µs    83486.        0B     33.4
#> 13 best                 5         100    9.2µs   10.3µs    93617.        0B     28.1
#> 14 worst                5         100  10.26µs   11.4µs    83996.        0B     33.6
#> 15 best                10         100    9.4µs   10.4µs    91849.        0B     27.6
#> 16 worst               10         100  11.69µs   12.8µs    75168.        0B     30.1
#> 17 best                50         100  10.74µs   11.8µs    81136.        0B     32.5
#> 18 worst               50         100  20.07µs   21.3µs    45752.        0B     13.7
#> 19 best               100         100  11.87µs   12.9µs    74278.        0B     29.7
#> 20 worst              100         100  30.91µs   32.7µs    28770.        0B     11.5
```
