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
#> 1 foo_S7(x)    7.61µs   8.85µs   106020.    10.8KB     21.2
#> 2 foo_S3(x)    2.48µs   2.75µs   327259.        0B      0  
#> 3 foo_S4(x)    2.69µs   3.13µs   308240.        0B     30.8

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
#> 1 bar_S7(x, y)   13.7µs  15.29µs    63200.        0B     19.0
#> 2 bar_S4(x, y)   7.08µs   8.08µs   120329.        0B     24.1
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
#>  1 best                 3          15   7.72µs    9.2µs   105379.        0B     21.1
#>  2 worst                3          15   7.83µs   9.36µs   103305.        0B     31.0
#>  3 best                 5          15   7.65µs   9.16µs   105244.        0B     31.6
#>  4 worst                5          15   7.91µs   9.37µs   103250.        0B     31.0
#>  5 best                10          15   7.83µs   9.27µs   103881.        0B     31.2
#>  6 worst               10          15   8.12µs   9.64µs   100361.        0B     20.1
#>  7 best                50          15   8.21µs   9.73µs    98875.        0B     29.7
#>  8 worst               50          15   10.1µs  11.62µs    83044.        0B     24.9
#>  9 best               100          15   9.04µs  10.49µs    92226.        0B     27.7
#> 10 worst              100          15  13.02µs  14.64µs    66093.        0B     19.8
#> 11 best                 3         100   7.76µs   9.28µs   103951.        0B     31.2
#> 12 worst                3         100   8.04µs   9.48µs   101895.        0B     30.6
#> 13 best                 5         100   7.79µs   9.25µs   104127.        0B     41.7
#> 14 worst                5         100   8.15µs   9.69µs    99960.        0B     20.0
#> 15 best                10         100   7.82µs   9.42µs   101685.        0B     20.3
#> 16 worst               10         100   8.61µs  10.22µs    93602.        0B     28.1
#> 17 best                50         100   8.32µs   9.86µs    96836.        0B     29.1
#> 18 worst               50         100  13.81µs  15.55µs    62023.        0B     18.6
#> 19 best               100         100   9.01µs  10.71µs    89141.        0B     26.8
#> 20 worst              100         100  20.61µs  22.53µs    42905.        0B     12.9
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
#>  1 best                 3          15   9.69µs   11.5µs    82919.        0B    24.9 
#>  2 worst                3          15   9.91µs   11.8µs    80719.        0B    32.3 
#>  3 best                 5          15   9.46µs   11.3µs    84198.        0B    33.7 
#>  4 worst                5          15   10.1µs   11.9µs    80123.        0B    24.0 
#>  5 best                10          15   9.71µs   11.6µs    82013.        0B    32.8 
#>  6 worst               10          15  10.28µs   11.6µs    82488.        0B    24.8 
#>  7 best                50          15  10.44µs   11.3µs    86687.        0B    26.0 
#>  8 worst               50          15  14.23µs   15.1µs    64522.        0B    25.8 
#>  9 best               100          15  11.66µs   12.5µs    77713.        0B    38.9 
#> 10 worst              100          15  19.44µs   20.7µs    46509.        0B    18.6 
#> 11 best                 3         100    9.9µs   11.3µs    83719.        0B    25.1 
#> 12 worst                3         100  10.59µs   12.1µs    79118.        0B    31.7 
#> 13 best                 5         100   9.66µs     11µs    86668.        0B    26.0 
#> 14 worst                5         100  10.72µs   12.1µs    78836.        0B    31.5 
#> 15 best                10         100   9.67µs   11.1µs    86444.        0B    25.9 
#> 16 worst               10         100  12.03µs   13.4µs    71193.        0B    28.5 
#> 17 best                50         100  10.95µs   12.3µs    77928.        0B    31.2 
#> 18 worst               50         100  21.06µs   22.4µs    43271.        0B    13.0 
#> 19 best               100         100  11.88µs   13.3µs    71179.        0B    35.6 
#> 20 worst              100         100  32.61µs   34.6µs    27864.        0B     8.36
```
