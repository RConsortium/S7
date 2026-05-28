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
#> 1 foo_S7(x)    7.41µs   8.46µs   107500.    10.8KB     21.5
#> 2 foo_S3(x)    2.48µs   2.67µs   340776.        0B      0  
#> 3 foo_S4(x)    2.73µs   3.01µs   319681.        0B     32.0

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
#> 1 bar_S7(x, y)   13.6µs  14.87µs    64790.        0B     19.4
#> 2 bar_S4(x, y)    7.1µs   7.75µs   125837.        0B     25.2
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
#>  1 best                 3          15    7.6µs   8.69µs   111780.        0B     22.4
#>  2 worst                3          15   7.68µs   8.76µs   111017.        0B     33.3
#>  3 best                 5          15   7.54µs   8.58µs   113098.        0B     33.9
#>  4 worst                5          15   7.88µs   8.97µs   108125.        0B     32.4
#>  5 best                10          15   7.56µs   8.68µs   112183.        0B     33.7
#>  6 worst               10          15   8.12µs   9.26µs   104880.        0B     21.0
#>  7 best                50          15    8.2µs   9.25µs   105031.        0B     31.5
#>  8 worst               50          15  10.04µs  11.08µs    87530.        0B     26.3
#>  9 best               100          15   8.67µs   9.73µs    99691.        0B     29.9
#> 10 worst              100          15  12.62µs  13.69µs    71202.        0B     21.4
#> 11 best                 3         100   7.46µs   8.53µs   113912.        0B     45.6
#> 12 worst                3         100   7.92µs   8.91µs   109206.        0B     32.8
#> 13 best                 5         100   7.52µs   8.59µs   112079.        0B     33.6
#> 14 worst                5         100   8.14µs   9.12µs   106069.        0B     31.8
#> 15 best                10         100   7.71µs   8.78µs   110080.        0B     33.0
#> 16 worst               10         100   8.57µs   9.52µs   101726.        0B     20.3
#> 17 best                50         100   8.12µs   9.18µs   105793.        0B     31.7
#> 18 worst               50         100  13.63µs  14.65µs    66366.        0B     19.9
#> 19 best               100         100   8.71µs    9.9µs    97989.        0B     29.4
#> 20 worst              100         100  20.56µs  21.75µs    44778.        0B     13.4
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
#>  1 best                 3          15   9.26µs   10.6µs    91454.        0B     36.6
#>  2 worst                3          15   9.69µs   10.8µs    89928.        0B     27.0
#>  3 best                 5          15   9.35µs   10.5µs    91932.        0B     36.8
#>  4 worst                5          15    9.8µs   10.9µs    88452.        0B     26.5
#>  5 best                10          15   9.45µs   10.7µs    89630.        0B     35.9
#>  6 worst               10          15  10.24µs     11µs    88136.        0B     26.4
#>  7 best                50          15  10.27µs   10.8µs    90211.        0B     27.1
#>  8 worst               50          15  13.99µs   14.6µs    66807.        0B     26.7
#>  9 best               100          15  11.53µs   12.1µs    80781.        0B     32.3
#> 10 worst              100          15  19.14µs   20.1µs    48048.        0B     19.2
#> 11 best                 3         100    9.5µs   10.5µs    91218.        0B     36.5
#> 12 worst                3         100   10.2µs   11.2µs    86091.        0B     25.8
#> 13 best                 5         100   9.34µs   10.3µs    92687.        0B     37.1
#> 14 worst                5         100  10.36µs   11.3µs    83226.        0B     25.0
#> 15 best                10         100   9.49µs   10.8µs    87460.        0B     35.0
#> 16 worst               10         100  11.74µs   12.8µs    75194.        0B     22.6
#> 17 best                50         100  10.74µs     12µs    76037.        0B     22.8
#> 18 worst               50         100  20.81µs   22.1µs    44009.        0B     17.6
#> 19 best               100         100   11.8µs   12.8µs    75698.        0B     30.3
#> 20 worst              100         100  32.31µs   33.5µs    29096.        0B     11.6
```
