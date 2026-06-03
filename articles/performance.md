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
#> 1 foo_S7(x)     7.3µs   8.94µs   103923.    10.8KB     20.8
#> 2 foo_S3(x)    2.51µs   2.89µs   315361.        0B      0  
#> 3 foo_S4(x)     2.7µs   3.25µs   294363.        0B     29.4

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
#> 1 bar_S7(x, y)  12.93µs  15.49µs    62564.        0B     18.8
#> 2 bar_S4(x, y)   6.94µs   8.08µs   120101.        0B     24.0
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
#>  1 best                 3          15   7.35µs   9.27µs   104044.        0B     20.8
#>  2 worst                3          15   7.55µs   9.13µs   105387.        0B     31.6
#>  3 best                 5          15   7.51µs   9.24µs   104011.        0B     31.2
#>  4 worst                5          15   7.79µs   9.54µs   101239.        0B     30.4
#>  5 best                10          15   7.55µs    9.2µs   104961.        0B     31.5
#>  6 worst               10          15   7.93µs   9.54µs   101109.        0B     20.2
#>  7 best                50          15   8.04µs   9.51µs   101964.        0B     30.6
#>  8 worst               50          15   9.76µs  11.26µs    86173.        0B     25.9
#>  9 best               100          15   8.74µs  10.15µs    95525.        0B     28.7
#> 10 worst              100          15  12.03µs  13.44µs    72575.        0B     21.8
#> 11 best                 3         100   7.54µs   9.14µs   105731.        0B     31.7
#> 12 worst                3         100   7.88µs    9.2µs   105255.        0B     31.6
#> 13 best                 5         100   7.66µs   9.02µs   107372.        0B     32.2
#> 14 worst                5         100   7.99µs   9.42µs   102487.        0B     20.5
#> 15 best                10         100   7.63µs   9.12µs   105092.        0B     21.0
#> 16 worst               10         100   8.27µs   9.82µs    97881.        0B     29.4
#> 17 best                50         100   8.14µs   9.73µs    97592.        0B     29.3
#> 18 worst               50         100  13.22µs  14.76µs    65993.        0B     13.2
#> 19 best               100         100    8.7µs  10.18µs    94851.        0B     28.5
#> 20 worst              100         100  19.54µs  21.45µs    45358.        0B     13.6
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
#>  1 best                 3          15    9.3µs   11.2µs    85601.        0B    25.7 
#>  2 worst                3          15   9.37µs   11.3µs    85000.        0B    34.0 
#>  3 best                 5          15   9.23µs   11.1µs    85904.        0B    34.4 
#>  4 worst                5          15   9.72µs   11.6µs    81958.        0B    24.6 
#>  5 best                10          15   9.41µs   11.2µs    85952.        0B    34.4 
#>  6 worst               10          15   9.97µs   11.1µs    85992.        0B    25.8 
#>  7 best                50          15  10.58µs   11.3µs    86703.        0B    34.7 
#>  8 worst               50          15  13.63µs   14.4µs    67879.        0B    27.2 
#>  9 best               100          15  11.66µs   12.5µs    78009.        0B    39.0 
#> 10 worst              100          15  18.26µs   19.7µs    49374.        0B    19.8 
#> 11 best                 3         100   9.39µs   10.8µs    89150.        0B    26.8 
#> 12 worst                3         100   10.2µs   11.8µs    80772.        0B    32.3 
#> 13 best                 5         100    9.4µs   10.8µs    88712.        0B    26.6 
#> 14 worst                5         100  10.37µs     12µs    79449.        0B    31.8 
#> 15 best                10         100   9.42µs   10.8µs    88136.        0B    26.4 
#> 16 worst               10         100   11.7µs   13.3µs    70030.        0B    28.0 
#> 17 best                50         100  10.68µs   12.4µs    72198.        0B    28.9 
#> 18 worst               50         100  20.01µs   21.8µs    44494.        0B    13.4 
#> 19 best               100         100  11.87µs   13.3µs    72279.        0B    28.9 
#> 20 worst              100         100  31.36µs   33.1µs    29505.        0B     8.85
```
