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
#> 1 foo_S7(x)    7.19µs   8.89µs   105047.    10.8KB     21.0
#> 2 foo_S3(x)    2.49µs   2.89µs   311317.        0B      0  
#> 3 foo_S4(x)     2.7µs   3.23µs   297535.        0B     29.8

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
#> 1 bar_S7(x, y)     13µs  15.14µs    64400.        0B     25.8
#> 2 bar_S4(x, y)    6.9µs   8.01µs   121048.        0B     24.2
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
#>  1 best                 3          15   7.34µs   9.05µs   106697.        0B     21.3
#>  2 worst                3          15   7.56µs   9.31µs   102861.        0B     30.9
#>  3 best                 5          15   7.41µs   9.19µs   103509.        0B     31.1
#>  4 worst                5          15    7.6µs   9.35µs   102383.        0B     30.7
#>  5 best                10          15   7.56µs   9.38µs   103067.        0B     30.9
#>  6 worst               10          15    7.9µs   9.69µs    99815.        0B     20.0
#>  7 best                50          15   8.05µs   9.93µs    97477.        0B     29.3
#>  8 worst               50          15   9.53µs  11.41µs    84755.        0B     25.4
#>  9 best               100          15   8.61µs  10.45µs    92467.        0B     27.7
#> 10 worst              100          15  11.81µs  13.67µs    70824.        0B     21.3
#> 11 best                 3         100   7.58µs    9.3µs   103770.        0B     31.1
#> 12 worst                3         100   7.87µs   9.55µs   100967.        0B     30.3
#> 13 best                 5         100   7.47µs   9.25µs   104314.        0B     41.7
#> 14 worst                5         100   7.93µs   9.71µs    99552.        0B     19.9
#> 15 best                10         100   7.55µs   9.37µs   102057.        0B     20.4
#> 16 worst               10         100   8.26µs  10.12µs    94294.        0B     28.3
#> 17 best                50         100   8.06µs   9.84µs    96775.        0B     29.0
#> 18 worst               50         100  13.02µs  14.85µs    65222.        0B     19.6
#> 19 best               100         100   8.73µs  10.54µs    90807.        0B     27.3
#> 20 worst              100         100  19.37µs  21.41µs    45369.        0B     13.6
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
#>  1 best                 3          15    9.2µs   11.5µs    82163.        0B     24.7
#>  2 worst                3          15    9.5µs   11.8µs    79778.        0B     31.9
#>  3 best                 5          15   9.24µs   11.5µs    83175.        0B     33.3
#>  4 worst                5          15   9.71µs   11.9µs    79471.        0B     23.8
#>  5 best                10          15   9.44µs   11.7µs    80985.        0B     32.4
#>  6 worst               10          15   9.88µs   11.3µs    84029.        0B     25.2
#>  7 best                50          15   10.2µs   11.1µs    87541.        0B     26.3
#>  8 worst               50          15  13.56µs   14.4µs    67736.        0B     27.1
#>  9 best               100          15  11.53µs   12.4µs    78214.        0B     31.3
#> 10 worst              100          15  18.14µs   19.6µs    48959.        0B     19.6
#> 11 best                 3         100   9.47µs   11.2µs    84856.        0B     25.5
#> 12 worst                3         100  10.23µs     12µs    79132.        0B     31.7
#> 13 best                 5         100   9.25µs   11.1µs    84967.        0B     25.5
#> 14 worst                5         100  10.29µs   12.3µs    76810.        0B     30.7
#> 15 best                10         100   9.43µs   11.3µs    83890.        0B     33.6
#> 16 worst               10         100  11.23µs   13.3µs    71316.        0B     21.4
#> 17 best                50         100  10.57µs   12.5µs    75775.        0B     30.3
#> 18 worst               50         100  19.91µs   21.8µs    44247.        0B     13.3
#> 19 best               100         100  11.78µs   13.8µs    68437.        0B     27.4
#> 20 worst              100         100  31.18µs   33.8µs    28709.        0B     11.5
```
