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
#> 1 foo_S7(x)    5.59µs   6.37µs   147503.    10.9KB     29.5
#> 2 foo_S3(x)    2.27µs   2.56µs   345946.        0B     34.6
#> 3 foo_S4(x)    2.45µs   2.78µs   337884.        0B     33.8

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
#> 1 bar_S7(x, y)  11.42µs  12.92µs    75039.        0B     30.0
#> 2 bar_S4(x, y)   6.43µs   7.34µs   132283.        0B     13.2
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
#>  1 best                 3          15   5.63µs   6.48µs   148443.        0B     29.7
#>  2 worst                3          15   5.84µs    6.7µs   143938.        0B     28.8
#>  3 best                 5          15   5.64µs    6.5µs   148031.        0B     29.6
#>  4 worst                5          15   5.82µs   6.75µs   142795.        0B     28.6
#>  5 best                10          15   5.71µs   6.57µs   147250.        0B     29.5
#>  6 worst               10          15   6.03µs   6.94µs   139156.        0B     13.9
#>  7 best                50          15    5.8µs   6.68µs   144053.        0B     28.8
#>  8 worst               50          15   7.34µs    8.2µs   118180.        0B     23.6
#>  9 best               100          15   6.08µs   6.92µs   139671.        0B     27.9
#> 10 worst              100          15   9.08µs   9.95µs    97065.        0B     19.4
#> 11 best                 3         100   5.75µs    6.6µs   145461.        0B     29.1
#> 12 worst                3         100   6.06µs   6.95µs   137836.        0B     27.6
#> 13 best                 5         100   5.76µs   6.61µs   144169.        0B     28.8
#> 14 worst                5         100   6.12µs   7.01µs   136742.        0B     27.4
#> 15 best                10         100   5.77µs   6.68µs   143411.        0B     28.7
#> 16 worst               10         100   6.38µs   7.28µs   132362.        0B     13.2
#> 17 best                50         100    5.8µs   6.66µs   143870.        0B     28.8
#> 18 worst               50         100  10.26µs  11.21µs    86569.        0B     17.3
#> 19 best               100         100    6.1µs   7.02µs   136807.        0B     27.4
#> 20 worst              100         100  15.33µs  16.45µs    59232.        0B     11.8
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
#>  1 best                 3          15   7.77µs   9.04µs   105126.        0B     31.5
#>  2 worst                3          15   8.12µs   9.43µs   101933.        0B     20.4
#>  3 best                 5          15   7.78µs   9.07µs   105210.        0B     31.6
#>  4 worst                5          15   8.34µs   9.52µs   100212.        0B     30.1
#>  5 best                10          15   7.91µs   9.15µs   104695.        0B     31.4
#>  6 worst               10          15   8.66µs   9.95µs    96403.        0B     19.3
#>  7 best                50          15   8.14µs   9.42µs   101254.        0B     30.4
#>  8 worst               50          15  11.08µs   12.4µs    77722.        0B     15.5
#>  9 best               100          15   8.56µs   9.82µs    97221.        0B     29.2
#> 10 worst              100          15   14.2µs  15.64µs    62117.        0B     18.6
#> 11 best                 3         100      8µs   9.35µs   102018.        0B     20.4
#> 12 worst                3         100   8.83µs  10.09µs    94659.        0B     28.4
#> 13 best                 5         100   7.93µs   9.18µs   103902.        0B     31.2
#> 14 worst                5         100   8.82µs  10.13µs    94362.        0B     28.3
#> 15 best                10         100   7.87µs   8.83µs   108654.        0B     21.7
#> 16 worst               10         100   9.82µs  10.35µs    93984.        0B     28.2
#> 17 best                50         100    8.4µs   8.87µs   109354.        0B     21.9
#> 18 worst               50         100  16.33µs  17.01µs    57732.        0B     17.3
#> 19 best               100         100   8.58µs   9.08µs   106828.        0B     21.4
#> 20 worst              100         100  25.17µs  25.84µs    37896.        0B     11.4
```
