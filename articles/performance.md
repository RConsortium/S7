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
#> 1 foo_S7(x)    7.44µs   9.23µs   101403.    10.8KB     20.3
#> 2 foo_S3(x)     2.5µs   2.94µs   308592.        0B      0  
#> 3 foo_S4(x)    2.73µs   3.32µs   289573.        0B     29.0

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
#> 1 bar_S7(x, y)  13.11µs  15.56µs    62374.        0B     18.7
#> 2 bar_S4(x, y)   6.99µs   8.21µs   118184.        0B     23.6
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
#>  1 best                 3          15   7.52µs   9.48µs   101333.        0B     20.3
#>  2 worst                3          15   7.82µs   9.79µs    98391.        0B     29.5
#>  3 best                 5          15   7.55µs   9.65µs    99882.        0B     30.0
#>  4 worst                5          15   7.73µs   9.76µs    98163.        0B     29.5
#>  5 best                10          15   7.59µs   9.63µs    99829.        0B     30.0
#>  6 worst               10          15   7.94µs  10.09µs    92146.        0B     18.4
#>  7 best                50          15   8.02µs  10.09µs    95967.        0B     28.8
#>  8 worst               50          15   9.74µs  11.69µs    82383.        0B     24.7
#>  9 best               100          15   8.67µs  10.76µs    89859.        0B     27.0
#> 10 worst              100          15  11.92µs  14.07µs    68909.        0B     20.7
#> 11 best                 3         100   7.48µs   9.58µs   100413.        0B     30.1
#> 12 worst                3         100   7.96µs   9.99µs    96365.        0B     28.9
#> 13 best                 5         100   7.66µs   9.72µs    99001.        0B     29.7
#> 14 worst                5         100   8.02µs  10.04µs    95199.        0B     28.6
#> 15 best                10         100    7.7µs   9.81µs    96914.        0B     19.4
#> 16 worst               10         100    8.4µs  10.69µs    88745.        0B     26.6
#> 17 best                50         100    8.1µs  10.36µs    92183.        0B     27.7
#> 18 worst               50         100  13.35µs  15.53µs    62229.        0B     12.4
#> 19 best               100         100   9.03µs  11.28µs    84647.        0B     25.4
#> 20 worst              100         100  19.73µs  21.93µs    44153.        0B     13.3
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
#>  1 best                 3          15   9.39µs   11.8µs    80281.        0B    24.1 
#>  2 worst                3          15   9.77µs   12.3µs    76647.        0B    30.7 
#>  3 best                 5          15   9.48µs   11.9µs    79753.        0B    23.9 
#>  4 worst                5          15    9.8µs   12.3µs    76504.        0B    30.6 
#>  5 best                10          15   9.73µs   12.1µs    78038.        0B    31.2 
#>  6 worst               10          15  10.12µs   11.7µs    80856.        0B    24.3 
#>  7 best                50          15  10.54µs   11.5µs    84433.        0B    33.8 
#>  8 worst               50          15  13.97µs     15µs    65160.        0B    19.6 
#>  9 best               100          15   11.9µs   13.1µs    73990.        0B    29.6 
#> 10 worst              100          15  18.58µs   20.1µs    48122.        0B    19.3 
#> 11 best                 3         100   9.53µs   11.5µs    82411.        0B    33.0 
#> 12 worst                3         100  10.45µs   12.4µs    77223.        0B    23.2 
#> 13 best                 5         100   9.37µs   11.3µs    83420.        0B    33.4 
#> 14 worst                5         100  10.59µs   12.5µs    75926.        0B    22.8 
#> 15 best                10         100   9.59µs   11.6µs    81908.        0B    24.6 
#> 16 worst               10         100   11.8µs   13.9µs    68599.        0B    27.5 
#> 17 best                50         100  11.02µs     13µs    73100.        0B    21.9 
#> 18 worst               50         100  20.26µs   22.6µs    42501.        0B    17.0 
#> 19 best               100         100  11.96µs   14.1µs    67119.        0B    33.6 
#> 20 worst              100         100  31.58µs   33.6µs    28881.        0B     8.67
```
