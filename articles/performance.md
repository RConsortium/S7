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
#> 1 foo_S7(x)    7.51µs   8.64µs   108218.    10.8KB     21.6
#> 2 foo_S3(x)    2.56µs    2.8µs   323972.        0B      0  
#> 3 foo_S4(x)    2.75µs   3.13µs   306901.        0B     30.7

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
#> 1 bar_S7(x, y)  13.95µs  15.45µs    62394.        0B     18.7
#> 2 bar_S4(x, y)   7.15µs   8.07µs   120277.        0B     24.1
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
#>  1 best                 3          15   7.81µs   9.08µs   106898.        0B     21.4
#>  2 worst                3          15   7.87µs   9.26µs   104424.        0B     31.3
#>  3 best                 5          15   7.63µs   8.92µs   108569.        0B     32.6
#>  4 worst                5          15   7.97µs   9.12µs   106188.        0B     31.9
#>  5 best                10          15   7.79µs   8.99µs   107745.        0B     32.3
#>  6 worst               10          15   8.19µs   9.29µs   104409.        0B     20.9
#>  7 best                50          15   8.24µs   9.52µs   101965.        0B     30.6
#>  8 worst               50          15   10.2µs  11.43µs    84668.        0B     25.4
#>  9 best               100          15   8.66µs   10.1µs    95880.        0B     28.8
#> 10 worst              100          15  12.73µs  14.09µs    68873.        0B     20.7
#> 11 best                 3         100   7.88µs   9.13µs   105944.        0B     31.8
#> 12 worst                3         100   8.15µs   9.36µs   103597.        0B     31.1
#> 13 best                 5         100   7.76µs   9.15µs   105511.        0B     42.2
#> 14 worst                5         100   8.21µs   9.57µs   100872.        0B     20.2
#> 15 best                10         100   8.02µs   9.39µs   102816.        0B     20.6
#> 16 worst               10         100   8.76µs  10.09µs    95473.        0B     28.7
#> 17 best                50         100   8.29µs    9.6µs   100617.        0B     30.2
#> 18 worst               50         100  13.81µs  15.18µs    63969.        0B     19.2
#> 19 best               100         100   9.09µs  10.41µs    92849.        0B     27.9
#> 20 worst              100         100  20.47µs  22.07µs    43853.        0B     13.2
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
#>  1 best                 3          15    9.6µs     11µs    87496.        0B    26.3 
#>  2 worst                3          15   9.88µs   11.2µs    85908.        0B    34.4 
#>  3 best                 5          15   9.69µs   11.2µs    86237.        0B    34.5 
#>  4 worst                5          15  10.21µs   11.8µs    82168.        0B    24.7 
#>  5 best                10          15   9.84µs   11.4µs    84634.        0B    33.9 
#>  6 worst               10          15  10.45µs   11.5µs    83858.        0B    25.2 
#>  7 best                50          15  10.54µs   11.2µs    86818.        0B    26.1 
#>  8 worst               50          15   14.3µs   15.2µs    64169.        0B    25.7 
#>  9 best               100          15   11.8µs   12.5µs    77488.        0B    38.8 
#> 10 worst              100          15  19.53µs   20.7µs    46744.        0B    18.7 
#> 11 best                 3         100   9.76µs   10.9µs    87921.        0B    26.4 
#> 12 worst                3         100  10.61µs   11.7µs    82815.        0B    33.1 
#> 13 best                 5         100   9.54µs   10.7µs    90719.        0B    27.2 
#> 14 worst                5         100  10.65µs   11.8µs    80689.        0B    32.3 
#> 15 best                10         100   9.66µs     11µs    87394.        0B    26.2 
#> 16 worst               10         100  11.99µs   13.3µs    72085.        0B    28.8 
#> 17 best                50         100  10.97µs   12.2µs    78685.        0B    31.5 
#> 18 worst               50         100  20.92µs   22.1µs    43810.        0B    13.1 
#> 19 best               100         100   11.9µs   13.1µs    73416.        0B    36.7 
#> 20 worst              100         100  32.52µs   34.3µs    28260.        0B     8.48
```
