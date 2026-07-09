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
#> 1 foo_S7(x)    7.98µs   9.76µs    97205.    10.8KB     29.2
#> 2 foo_S3(x)    2.47µs   2.91µs   312311.        0B      0  
#> 3 foo_S4(x)    2.66µs   3.17µs   302551.        0B     30.3

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
#> 1 bar_S7(x, y)  14.56µs  17.21µs    56525.        0B     22.6
#> 2 bar_S4(x, y)   6.95µs   8.13µs   119804.        0B     24.0
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
#>  1 best                 3          15   8.26µs  10.03µs    96423.        0B     28.9
#>  2 worst                3          15    8.3µs  10.16µs    94953.        0B     28.5
#>  3 best                 5          15   8.36µs  10.12µs    95371.        0B     28.6
#>  4 worst                5          15   8.44µs  10.26µs    94271.        0B     28.3
#>  5 best                10          15   8.18µs   9.98µs    96957.        0B     29.1
#>  6 worst               10          15   8.62µs   10.3µs    94323.        0B     28.3
#>  7 best                50          15   8.82µs  10.67µs    90766.        0B     36.3
#>  8 worst               50          15   10.5µs  12.37µs    78418.        0B     23.5
#>  9 best               100          15   9.43µs  11.46µs    84105.        0B     33.7
#> 10 worst              100          15  12.78µs  14.87µs    65282.        0B     26.1
#> 11 best                 3         100   8.28µs  10.22µs    93956.        0B     28.2
#> 12 worst                3         100    8.6µs  10.67µs    89343.        0B     35.8
#> 13 best                 5         100   8.37µs   10.6µs    89689.        0B     26.9
#> 14 worst                5         100   8.51µs  10.72µs    88809.        0B     26.7
#> 15 best                10         100   8.33µs  10.44µs    91226.        0B     27.4
#> 16 worst               10         100   8.92µs  11.07µs    85211.        0B     25.6
#> 17 best                50         100   8.87µs  10.78µs    87492.        0B     35.0
#> 18 worst               50         100  14.01µs  15.97µs    60412.        0B     18.1
#> 19 best               100         100   9.33µs  11.41µs    83250.        0B     33.3
#> 20 worst              100         100  20.01µs  22.28µs    43448.        0B     17.4
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
#>  1 best                 3          15   10.5µs   12.1µs    78216.        0B     39.1
#>  2 worst                3          15   10.9µs     12µs    81307.        0B     32.5
#>  3 best                 5          15   10.7µs   11.8µs    82455.        0B     41.2
#>  4 worst                5          15   11.2µs   12.2µs    79870.        0B     32.0
#>  5 best                10          15   10.8µs   11.9µs    81622.        0B     40.8
#>  6 worst               10          15   11.4µs   12.8µs    75532.        0B     30.2
#>  7 best                50          15     12µs   13.4µs    72187.        0B     28.9
#>  8 worst               50          15   15.2µs   16.6µs    58578.        0B     29.3
#>  9 best               100          15   13.4µs   15.3µs    61512.        0B     30.8
#> 10 worst              100          15     20µs     22µs    43109.        0B     17.3
#> 11 best                 3         100   11.1µs   13.1µs    71027.        0B     35.5
#> 12 worst                3         100   11.9µs   13.8µs    67765.        0B     27.1
#> 13 best                 5         100     11µs   12.8µs    72643.        0B     29.1
#> 14 worst                5         100   12.1µs     14µs    66651.        0B     33.3
#> 15 best                10         100   11.1µs     13µs    71913.        0B     36.0
#> 16 worst               10         100   13.2µs   15.2µs    61493.        0B     24.6
#> 17 best                50         100   12.4µs   14.4µs    64542.        0B     32.3
#> 18 worst               50         100   21.9µs   24.2µs    36223.        0B     14.5
#> 19 best               100         100   13.3µs   15.4µs    60604.        0B     36.4
#> 20 worst              100         100     33µs   35.8µs    26901.        0B     13.5
```
