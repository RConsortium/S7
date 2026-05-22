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
#> 1 foo_S7(x)    7.06µs   8.63µs   108306.    18.2KB     21.7
#> 2 foo_S3(x)    2.49µs   2.85µs   319907.        0B      0  
#> 3 foo_S4(x)     2.7µs   3.21µs   299976.        0B     30.0

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
#> 1 bar_S7(x, y)     13µs  15.17µs    56969.        0B     17.1
#> 2 bar_S4(x, y)      7µs   8.69µs    86382.        0B     17.3
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
#>  1 best                 3          15   7.19µs   8.77µs   110569.        0B     22.1
#>  2 worst                3          15   7.45µs   8.99µs   107722.        0B     32.3
#>  3 best                 5          15   7.31µs   8.92µs   108348.        0B     32.5
#>  4 worst                5          15    7.5µs   9.14µs   105773.        0B     31.7
#>  5 best                10          15   7.41µs   8.99µs   107372.        0B     32.2
#>  6 worst               10          15   7.79µs   9.42µs   102886.        0B     20.6
#>  7 best                50          15   7.85µs   9.53µs   101692.        0B     30.5
#>  8 worst               50          15   9.55µs  11.29µs    85710.        0B     25.7
#>  9 best               100          15   8.63µs  10.37µs    93422.        0B     28.0
#> 10 worst              100          15  11.88µs  13.57µs    71761.        0B     21.5
#> 11 best                 3         100   7.48µs   8.96µs   108030.        0B     32.4
#> 12 worst                3         100   7.84µs   9.38µs   102908.        0B     30.9
#> 13 best                 5         100   7.57µs   9.11µs   106324.        0B     31.9
#> 14 worst                5         100   7.73µs   9.43µs   102572.        0B     30.8
#> 15 best                10         100    7.4µs   9.12µs   104492.        0B     20.9
#> 16 worst               10         100   8.13µs   9.86µs    97089.        0B     29.1
#> 17 best                50         100   7.94µs   9.62µs    99194.        0B     29.8
#> 18 worst               50         100   13.2µs  14.85µs    65409.        0B     13.1
#> 19 best               100         100   8.64µs  10.39µs    92262.        0B     27.7
#> 20 worst              100         100  19.43µs  21.29µs    45683.        0B     13.7
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
#>  1 best                 3          15   9.03µs     11µs    86492.        0B     26.0
#>  2 worst                3          15   9.22µs   11.4µs    83072.        0B     33.2
#>  3 best                 5          15      9µs     11µs    86868.        0B     26.1
#>  4 worst                5          15   9.38µs   11.5µs    82644.        0B     33.1
#>  5 best                10          15    9.1µs   11.2µs    85047.        0B     34.0
#>  6 worst               10          15   9.63µs   11.1µs    84645.        0B     25.4
#>  7 best                50          15  10.19µs   11.1µs    87892.        0B     35.2
#>  8 worst               50          15  13.41µs   14.3µs    68562.        0B     20.6
#>  9 best               100          15  11.58µs   12.5µs    77737.        0B     31.1
#> 10 worst              100          15  18.18µs   19.3µs    50020.        0B     20.0
#> 11 best                 3         100   9.26µs   10.6µs    91154.        0B     27.4
#> 12 worst                3         100   9.95µs   11.3µs    85525.        0B     34.2
#> 13 best                 5         100   9.09µs   10.4µs    92728.        0B     27.8
#> 14 worst                5         100  10.12µs   11.5µs    83855.        0B     33.6
#> 15 best                10         100   9.31µs   10.6µs    90768.        0B     27.2
#> 16 worst               10         100  11.46µs   12.8µs    75753.        0B     30.3
#> 17 best                50         100  10.51µs   11.9µs    81467.        0B     32.6
#> 18 worst               50         100  20.04µs   21.4µs    45693.        0B     13.7
#> 19 best               100         100  11.74µs   12.9µs    74928.        0B     30.0
#> 20 worst              100         100  31.01µs   32.5µs    30126.        0B     12.1
```
