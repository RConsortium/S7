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
#> 1 foo_S7(x)    7.55µs   9.12µs   102820.    10.8KB     20.6
#> 2 foo_S3(x)    2.48µs   2.78µs   325101.        0B      0  
#> 3 foo_S4(x)    2.69µs    3.1µs   309606.        0B     31.0

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
#> 1 bar_S7(x, y)  13.73µs  15.55µs    62055.        0B     18.6
#> 2 bar_S4(x, y)   7.26µs   8.21µs   118654.        0B     23.7
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
#>  1 best                 3          15   7.43µs   8.99µs   107335.        0B     21.5
#>  2 worst                3          15   7.83µs   9.11µs   106323.        0B     31.9
#>  3 best                 5          15   7.58µs   9.03µs   107373.        0B     32.2
#>  4 worst                5          15   7.72µs   9.09µs   106118.        0B     31.8
#>  5 best                10          15   7.73µs   9.15µs   105928.        0B     31.8
#>  6 worst               10          15   8.12µs   9.44µs   102384.        0B     20.5
#>  7 best                50          15   8.12µs    9.6µs   100473.        0B     30.2
#>  8 worst               50          15   9.96µs   11.4µs    84914.        0B     25.5
#>  9 best               100          15   8.78µs  10.12µs    95794.        0B     28.7
#> 10 worst              100          15  12.96µs  14.48µs    66674.        0B     20.0
#> 11 best                 3         100   7.56µs   9.06µs   106532.        0B     32.0
#> 12 worst                3         100    8.1µs   9.39µs   103084.        0B     30.9
#> 13 best                 5         100   7.61µs   8.99µs   107589.        0B     43.1
#> 14 worst                5         100   8.23µs   9.65µs    99810.        0B     20.0
#> 15 best                10         100   7.78µs    9.3µs   103610.        0B     20.7
#> 16 worst               10         100   8.63µs  10.11µs    94873.        0B     28.5
#> 17 best                50         100   8.13µs   9.69µs    98887.        0B     29.7
#> 18 worst               50         100  13.86µs  15.34µs    62975.        0B     18.9
#> 19 best               100         100   8.86µs  10.49µs    90685.        0B     27.2
#> 20 worst              100         100  20.39µs  22.06µs    43883.        0B     13.2
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
#>  1 best                 3          15   9.44µs   11.3µs    84771.        0B    25.4 
#>  2 worst                3          15   9.83µs   11.6µs    82161.        0B    32.9 
#>  3 best                 5          15   9.45µs   11.1µs    85675.        0B    34.3 
#>  4 worst                5          15   9.66µs   11.6µs    82226.        0B    24.7 
#>  5 best                10          15   9.66µs   11.4µs    83937.        0B    33.6 
#>  6 worst               10          15  10.12µs   11.4µs    83728.        0B    25.1 
#>  7 best                50          15  10.34µs   11.2µs    81506.        0B    24.5 
#>  8 worst               50          15  14.13µs   15.1µs    64361.        0B    25.8 
#>  9 best               100          15  11.42µs   12.4µs    78356.        0B    39.2 
#> 10 worst              100          15  19.27µs   20.5µs    46787.        0B    18.7 
#> 11 best                 3         100   9.73µs     11µs    86183.        0B    25.9 
#> 12 worst                3         100  10.48µs   11.9µs    80839.        0B    32.3 
#> 13 best                 5         100    9.4µs   10.7µs    89457.        0B    26.8 
#> 14 worst                5         100  10.49µs   11.8µs    81382.        0B    32.6 
#> 15 best                10         100   9.54µs   10.8µs    88633.        0B    26.6 
#> 16 worst               10         100  11.85µs   13.3µs    72041.        0B    28.8 
#> 17 best                50         100  10.91µs   12.3µs    77271.        0B    30.9 
#> 18 worst               50         100  20.73µs   22.1µs    43744.        0B    13.1 
#> 19 best               100         100  11.82µs   13.4µs    71157.        0B    35.6 
#> 20 worst              100         100  32.47µs   34.5µs    28078.        0B     8.43
```
