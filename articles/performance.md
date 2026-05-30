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
#> 1 foo_S7(x)    7.49µs   8.62µs   107505.    10.8KB     21.5
#> 2 foo_S3(x)    2.54µs   2.81µs   325215.        0B      0  
#> 3 foo_S4(x)    2.69µs      3µs   316546.        0B     31.7

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
#> 1 bar_S7(x, y)  13.23µs  14.99µs    58889.        0B     23.6
#> 2 bar_S4(x, y)   7.05µs   7.83µs   124072.        0B     24.8
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
#>  1 best                 3          15   7.63µs    8.9µs   108981.        0B     21.8
#>  2 worst                3          15   7.66µs   8.71µs   111416.        0B     33.4
#>  3 best                 5          15   7.61µs   8.65µs   112243.        0B     33.7
#>  4 worst                5          15   7.83µs   8.78µs   110812.        0B     33.3
#>  5 best                10          15   7.66µs   8.75µs   110691.        0B     33.2
#>  6 worst               10          15   8.04µs   9.14µs   106311.        0B     21.3
#>  7 best                50          15   8.21µs   9.53µs   101765.        0B     30.5
#>  8 worst               50          15  10.08µs  11.41µs    84937.        0B     25.5
#>  9 best               100          15   8.75µs   9.99µs    97259.        0B     29.2
#> 10 worst              100          15  12.68µs  13.89µs    70051.        0B     21.0
#> 11 best                 3         100   7.69µs   8.95µs   108262.        0B     32.5
#> 12 worst                3         100   8.01µs   9.23µs   104431.        0B     31.3
#> 13 best                 5         100   7.67µs   8.77µs   110521.        0B     33.2
#> 14 worst                5         100   8.12µs   9.15µs   105921.        0B     31.8
#> 15 best                10         100   7.66µs   8.86µs   108681.        0B     21.7
#> 16 worst               10         100   8.47µs   9.75µs    97172.        0B     29.2
#> 17 best                50         100    8.1µs   9.38µs   103070.        0B     30.9
#> 18 worst               50         100  13.56µs   14.8µs    65834.        0B     13.2
#> 19 best               100         100   8.86µs  10.13µs    95651.        0B     28.7
#> 20 worst              100         100  20.47µs  21.91µs    44259.        0B     13.3
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
#>  1 best                 3          15   9.32µs     11µs    87607.        0B     26.3
#>  2 worst                3          15    9.7µs   11.2µs    85246.        0B     34.1
#>  3 best                 5          15   9.41µs     11µs    86995.        0B     26.1
#>  4 worst                5          15   9.75µs   11.4µs    84208.        0B     33.7
#>  5 best                10          15   9.49µs   10.9µs    88263.        0B     26.5
#>  6 worst               10          15  10.11µs   11.3µs    85534.        0B     34.2
#>  7 best                50          15  10.32µs     11µs    87992.        0B     35.2
#>  8 worst               50          15  14.02µs   14.7µs    65907.        0B     19.8
#>  9 best               100          15  11.59µs   12.4µs    78323.        0B     31.3
#> 10 worst              100          15  19.25µs   20.2µs    48029.        0B     19.2
#> 11 best                 3         100   9.66µs   10.7µs    90149.        0B     27.1
#> 12 worst                3         100   10.4µs   11.5µs    84174.        0B     33.7
#> 13 best                 5         100   9.38µs   10.3µs    93407.        0B     28.0
#> 14 worst                5         100  10.45µs   11.6µs    83459.        0B     33.4
#> 15 best                10         100   9.58µs   10.8µs    89143.        0B     26.8
#> 16 worst               10         100  11.96µs   13.2µs    73074.        0B     29.2
#> 17 best                50         100  10.82µs     12µs    79838.        0B     31.9
#> 18 worst               50         100  20.84µs   22.1µs    43819.        0B     13.1
#> 19 best               100         100  11.76µs     13µs    73562.        0B     29.4
#> 20 worst              100         100  32.24µs   33.7µs    28828.        0B     11.5
```
