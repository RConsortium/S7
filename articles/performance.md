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
#> 1 foo_S7(x)    7.53µs   8.97µs   104344.    10.8KB     20.9
#> 2 foo_S3(x)     2.5µs   2.79µs   322590.        0B      0  
#> 3 foo_S4(x)    2.67µs   3.12µs   307519.        0B     30.8

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
#> 1 bar_S7(x, y)  13.57µs  15.39µs    62408.        0B     18.7
#> 2 bar_S4(x, y)   7.14µs   8.06µs   120252.        0B     24.1
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
#>  1 best                 3          15   7.55µs   9.15µs   105743.        0B     21.2
#>  2 worst                3          15   7.81µs   9.37µs   102959.        0B     30.9
#>  3 best                 5          15   7.45µs   9.05µs   106533.        0B     32.0
#>  4 worst                5          15   7.83µs    9.4µs   102754.        0B     30.8
#>  5 best                10          15   7.58µs   9.06µs   106445.        0B     31.9
#>  6 worst               10          15   8.14µs   9.64µs   100604.        0B     20.1
#>  7 best                50          15   8.22µs   9.73µs    99462.        0B     29.8
#>  8 worst               50          15  10.13µs  11.62µs    83492.        0B     25.1
#>  9 best               100          15   8.76µs  10.33µs    93791.        0B     28.1
#> 10 worst              100          15  12.78µs  14.46µs    67076.        0B     20.1
#> 11 best                 3         100   7.62µs    9.2µs   104548.        0B     31.4
#> 12 worst                3         100   7.99µs   9.53µs   101474.        0B     40.6
#> 13 best                 5         100   7.63µs    9.1µs   105418.        0B     31.6
#> 14 worst                5         100   8.12µs   9.74µs    98398.        0B     29.5
#> 15 best                10         100   7.69µs   9.32µs   103018.        0B     30.9
#> 16 worst               10         100   8.54µs  10.19µs    94182.        0B     18.8
#> 17 best                50         100   8.01µs   9.64µs   100074.        0B     30.0
#> 18 worst               50         100  13.78µs   15.5µs    62166.        0B     18.7
#> 19 best               100         100   8.93µs   10.4µs    92838.        0B     27.9
#> 20 worst              100         100  20.49µs  22.14µs    43890.        0B     13.2
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
#>  1 best                 3          15   9.32µs   11.2µs    84850.        0B    34.0 
#>  2 worst                3          15   9.88µs   11.7µs    81808.        0B    24.5 
#>  3 best                 5          15   9.48µs   11.3µs    83978.        0B    33.6 
#>  4 worst                5          15   9.96µs   11.8µs    80915.        0B    24.3 
#>  5 best                10          15   9.56µs   11.4µs    83064.        0B    33.2 
#>  6 worst               10          15  10.12µs   11.1µs    85618.        0B    25.7 
#>  7 best                50          15   10.4µs   11.2µs    86516.        0B    26.0 
#>  8 worst               50          15  14.06µs     15µs    64499.        0B    25.8 
#>  9 best               100          15  11.53µs   12.4µs    78677.        0B    31.5 
#> 10 worst              100          15  19.31µs   20.8µs    46461.        0B    18.6 
#> 11 best                 3         100   9.62µs   11.1µs    86954.        0B    26.1 
#> 12 worst                3         100  10.42µs     12µs    79869.        0B    32.0 
#> 13 best                 5         100   9.55µs   11.1µs    85292.        0B    25.6 
#> 14 worst                5         100  10.45µs   12.3µs    77890.        0B    31.2 
#> 15 best                10         100   9.65µs   11.1µs    86311.        0B    34.5 
#> 16 worst               10         100  12.01µs   13.5µs    71160.        0B    21.4 
#> 17 best                50         100  10.79µs   12.4µs    77205.        0B    30.9 
#> 18 worst               50         100  20.73µs   22.2µs    43453.        0B    13.0 
#> 19 best               100         100  11.74µs   13.6µs    70450.        0B    35.2 
#> 20 worst              100         100   32.6µs   34.6µs    27991.        0B     8.40
```
