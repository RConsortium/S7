# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text := new_class(parent = class_character)
Number := new_class(parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 := new_generic("x")
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
#> 1 foo_S7(x)    4.71µs   5.76µs   158636.    10.9KB     31.7
#> 2 foo_S3(x)    1.95µs   2.45µs   352236.        0B     35.2
#> 3 foo_S4(x)    2.09µs   2.67µs   341315.        0B     34.1

bar_S7 := new_generic(c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)   9.68µs  11.83µs    79935.        0B     24.0
#> 2 bar_S4(x, y)   5.59µs   6.73µs   141480.        0B     14.1
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
    Text := new_class(parent = class_character)
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
    foo_S7 := new_generic("x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic("x")
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
#>  1 best                 3          15   4.72µs    5.9µs   159264.        0B     31.9
#>  2 worst                3          15   4.88µs   6.13µs   153512.        0B     30.7
#>  3 best                 5          15   4.76µs   5.94µs   157367.        0B     31.5
#>  4 worst                5          15   5.02µs   6.26µs   149977.        0B     30.0
#>  5 best                10          15   4.77µs      6µs   156867.        0B     31.4
#>  6 worst               10          15   5.15µs   6.42µs   147519.        0B     14.8
#>  7 best                50          15   4.98µs   6.15µs   152302.        0B     30.5
#>  8 worst               50          15   6.68µs   7.91µs   120056.        0B     24.0
#>  9 best               100          15   5.13µs   6.37µs   148156.        0B     29.6
#> 10 worst              100          15   8.58µs   9.85µs    97040.        0B     19.4
#> 11 best                 3         100   4.85µs   6.06µs   153788.        0B     30.8
#> 12 worst                3         100   5.18µs   6.46µs   145221.        0B     29.0
#> 13 best                 5         100   4.84µs   6.08µs   151432.        0B     30.3
#> 14 worst                5         100   5.21µs    6.5µs   141355.        0B     28.3
#> 15 best                10         100   4.83µs   6.08µs   153458.        0B     30.7
#> 16 worst               10         100   5.54µs   6.79µs   139109.        0B     13.9
#> 17 best                50         100   4.92µs   6.17µs   149586.        0B     29.9
#> 18 worst               50         100   9.63µs  11.06µs    85701.        0B     17.1
#> 19 best               100         100   5.19µs   6.45µs   145214.        0B     29.0
#> 20 worst              100         100  14.97µs  16.69µs    58064.        0B     11.6
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text := new_class(parent = class_character)
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
    foo_S7 := new_generic(c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 := new_generic(c("x", "y"))
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
#>  1 best                 3          15   6.54µs   8.42µs   109698.        0B    32.9 
#>  2 worst                3          15   6.97µs   8.88µs   106461.        0B    21.3 
#>  3 best                 5          15   6.62µs   8.43µs   110553.        0B    22.1 
#>  4 worst                5          15   7.04µs   8.87µs   104407.        0B    31.3 
#>  5 best                10          15   6.74µs   8.52µs   108564.        0B    32.6 
#>  6 worst               10          15   7.53µs   9.42µs   100251.        0B    20.1 
#>  7 best                50          15   6.97µs   8.79µs   105834.        0B    31.8 
#>  8 worst               50          15  10.31µs   12.4µs    77161.        0B    15.4 
#>  9 best               100          15   7.31µs   9.29µs    99717.        0B    19.9 
#> 10 worst              100          15   14.2µs  16.36µs    58538.        0B    17.6 
#> 11 best                 3         100   7.02µs   8.73µs   105135.        0B    31.5 
#> 12 worst                3         100   7.62µs   9.57µs    98897.        0B    19.8 
#> 13 best                 5         100   6.69µs   8.61µs   108849.        0B    21.8 
#> 14 worst                5         100   7.77µs   9.63µs    96350.        0B    28.9 
#> 15 best                10         100   6.71µs   7.84µs   118322.        0B    35.5 
#> 16 worst               10         100   8.72µs   9.66µs    98672.        0B    19.7 
#> 17 best                50         100   7.14µs   8.05µs   118637.        0B    35.6 
#> 18 worst               50         100  15.94µs  16.66µs    58393.        0B    11.7 
#> 19 best               100         100   7.62µs   8.32µs   115038.        0B    34.5 
#> 20 worst              100         100  24.73µs  26.26µs    37313.        0B     7.46
```
