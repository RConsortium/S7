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
#> 1 foo_S7(x)    7.47µs   8.78µs   106156.    10.8KB     21.2
#> 2 foo_S3(x)    2.49µs   2.78µs   328551.        0B      0  
#> 3 foo_S4(x)    2.71µs   3.14µs   295091.        0B     29.5

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
#> 1 bar_S7(x, y)  13.62µs  15.26µs    63172.        0B     19.0
#> 2 bar_S4(x, y)   7.22µs   8.11µs   120140.        0B     24.0
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
#>  1 best                 3          15   7.64µs   9.08µs   100430.        0B     20.1
#>  2 worst                3          15   7.97µs   9.25µs   100849.        0B     30.3
#>  3 best                 5          15   7.65µs   9.18µs    90917.        0B     27.3
#>  4 worst                5          15   7.94µs   9.29µs   103322.        0B     31.0
#>  5 best                10          15   7.72µs   9.02µs   107155.        0B     32.2
#>  6 worst               10          15   8.09µs   9.36µs   103566.        0B     20.7
#>  7 best                50          15   7.96µs   9.43µs    94755.        0B     28.4
#>  8 worst               50          15     10µs  11.45µs    82652.        0B     24.8
#>  9 best               100          15   8.95µs  10.29µs    94026.        0B     28.2
#> 10 worst              100          15     13µs  14.39µs    67170.        0B     20.2
#> 11 best                 3         100   7.75µs   9.14µs   105677.        0B     31.7
#> 12 worst                3         100   8.07µs   9.56µs   100639.        0B     30.2
#> 13 best                 5         100    7.8µs   9.15µs   105682.        0B     31.7
#> 14 worst                5         100   8.06µs   9.45µs   102201.        0B     30.7
#> 15 best                10         100   7.79µs   9.18µs   104282.        0B     20.9
#> 16 worst               10         100   8.54µs  10.07µs    95273.        0B     28.6
#> 17 best                50         100   8.11µs   9.65µs    99826.        0B     30.0
#> 18 worst               50         100  13.87µs  15.25µs    63612.        0B     12.7
#> 19 best               100         100   8.95µs  10.48µs    91614.        0B     27.5
#> 20 worst              100         100  20.73µs  22.28µs    43384.        0B     13.0
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
#>  1 best                 3          15    9.3µs     11µs    87042.        0B    26.1 
#>  2 worst                3          15   9.71µs   11.3µs    85657.        0B    34.3 
#>  3 best                 5          15   9.52µs   10.9µs    88786.        0B    26.6 
#>  4 worst                5          15  10.01µs   11.5µs    84263.        0B    33.7 
#>  5 best                10          15   9.68µs     11µs    87499.        0B    35.0 
#>  6 worst               10          15  10.26µs   11.4µs    84760.        0B    25.4 
#>  7 best                50          15   10.4µs   11.1µs    86207.        0B    34.5 
#>  8 worst               50          15  14.11µs   14.9µs    65186.        0B    19.6 
#>  9 best               100          15  11.62µs   12.5µs    77639.        0B    31.1 
#> 10 worst              100          15  19.46µs   20.7µs    46209.        0B    18.5 
#> 11 best                 3         100   9.61µs     11µs    87111.        0B    34.9 
#> 12 worst                3         100   10.5µs   11.7µs    82728.        0B    24.8 
#> 13 best                 5         100   9.69µs     11µs    86691.        0B    34.7 
#> 14 worst                5         100  10.64µs   11.9µs    80369.        0B    24.1 
#> 15 best                10         100   9.65µs   10.8µs    89090.        0B    26.7 
#> 16 worst               10         100  11.83µs   13.2µs    73102.        0B    29.3 
#> 17 best                50         100  10.84µs     12µs    80566.        0B    24.2 
#> 18 worst               50         100  20.99µs   22.4µs    43177.        0B    17.3 
#> 19 best               100         100  12.06µs   13.5µs    71070.        0B    35.6 
#> 20 worst              100         100  32.57µs   34.2µs    28340.        0B     8.50
```
