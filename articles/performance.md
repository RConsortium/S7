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
#> 1 foo_S7(x)    7.18µs   8.73µs   107032.    10.8KB     21.4
#> 2 foo_S3(x)    2.51µs   2.87µs   317040.        0B      0  
#> 3 foo_S4(x)    2.71µs   3.27µs   293674.        0B     29.4

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
#> 1 bar_S7(x, y)     13µs  15.19µs    63929.        0B     19.2
#> 2 bar_S4(x, y)      7µs   8.14µs   118961.        0B     23.8
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
#>  1 best                 3          15   7.47µs   9.09µs   106753.        0B     21.4
#>  2 worst                3          15   7.76µs   9.39µs   103098.        0B     30.9
#>  3 best                 5          15   7.55µs   9.21µs   104943.        0B     31.5
#>  4 worst                5          15   7.82µs   9.41µs   102743.        0B     30.8
#>  5 best                10          15   7.62µs   9.31µs   103932.        0B     31.2
#>  6 worst               10          15      8µs   9.66µs   100601.        0B     20.1
#>  7 best                50          15   8.12µs   9.81µs    98530.        0B     29.6
#>  8 worst               50          15   9.77µs  11.42µs    84842.        0B     25.5
#>  9 best               100          15   8.66µs  10.42µs    93046.        0B     27.9
#> 10 worst              100          15  11.97µs  13.69µs    71169.        0B     21.4
#> 11 best                 3         100   7.58µs    9.3µs   103947.        0B     31.2
#> 12 worst                3         100   7.97µs   9.68µs   100180.        0B     30.1
#> 13 best                 5         100   7.51µs   9.27µs   103403.        0B     41.4
#> 14 worst                5         100   7.89µs   9.57µs   101031.        0B     20.2
#> 15 best                10         100   7.51µs   9.41µs   102255.        0B     20.5
#> 16 worst               10         100   8.47µs  10.24µs    93572.        0B     28.1
#> 17 best                50         100   8.08µs   9.86µs    97110.        0B     29.1
#> 18 worst               50         100  13.28µs  15.08µs    64322.        0B     19.3
#> 19 best               100         100   8.84µs  10.69µs    89582.        0B     26.9
#> 20 worst              100         100  19.76µs  21.68µs    44903.        0B     13.5
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
#>  1 best                 3          15    9.4µs   11.5µs    83359.        0B    25.0 
#>  2 worst                3          15   9.51µs   11.6µs    81826.        0B    32.7 
#>  3 best                 5          15   9.33µs   11.5µs    82741.        0B    33.1 
#>  4 worst                5          15    9.9µs     12µs    79356.        0B    23.8 
#>  5 best                10          15    9.6µs   11.7µs    81162.        0B    32.5 
#>  6 worst               10          15  10.17µs   11.5µs    83316.        0B    25.0 
#>  7 best                50          15  10.37µs   11.4µs    85901.        0B    25.8 
#>  8 worst               50          15  13.73µs   14.5µs    67269.        0B    26.9 
#>  9 best               100          15  11.81µs   12.8µs    76576.        0B    38.3 
#> 10 worst              100          15  18.43µs   19.7µs    49094.        0B    19.6 
#> 11 best                 3         100   9.44µs   10.9µs    87319.        0B    26.2 
#> 12 worst                3         100   10.5µs   12.1µs    79045.        0B    31.6 
#> 13 best                 5         100    9.5µs   10.8µs    88164.        0B    26.5 
#> 14 worst                5         100  10.54µs   12.1µs    78268.        0B    31.3 
#> 15 best                10         100    9.5µs   10.9µs    87178.        0B    26.2 
#> 16 worst               10         100  11.81µs   13.3µs    70696.        0B    28.3 
#> 17 best                50         100  10.73µs   12.4µs    76935.        0B    30.8 
#> 18 worst               50         100  20.11µs   21.7µs    44579.        0B    13.4 
#> 19 best               100         100     12µs   13.5µs    70362.        0B    35.2 
#> 20 worst              100         100  31.08µs   33.1µs    29473.        0B     8.84
```
