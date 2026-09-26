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
#> 1 foo_S7(x)    4.62µs   5.57µs   165124.    10.8KB     16.5
#> 2 foo_S3(x)    1.94µs   2.41µs   367558.        0B     36.8
#> 3 foo_S4(x)    2.11µs   2.66µs   351163.        0B     35.1

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
#> 1 bar_S7(x, y)    9.5µs   11.3µs    85417.        0B     25.6
#> 2 bar_S4(x, y)   5.58µs   6.51µs   147180.        0B     29.4
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
#>  1 best                 3          15   4.67µs   5.64µs   168903.        0B    33.8 
#>  2 worst                3          15   4.82µs   5.75µs   164838.        0B    33.0 
#>  3 best                 5          15   4.66µs   5.63µs   168498.        0B    33.7 
#>  4 worst                5          15   4.88µs   5.81µs   163414.        0B    32.7 
#>  5 best                10          15   4.71µs   5.72µs   165882.        0B    16.6 
#>  6 worst               10          15   5.05µs   6.07µs   156983.        0B    31.4 
#>  7 best                50          15   4.85µs   5.85µs   161760.        0B    32.4 
#>  8 worst               50          15   6.41µs   7.42µs   129320.        0B    12.9 
#>  9 best               100          15   5.05µs   6.12µs   155570.        0B    31.1 
#> 10 worst              100          15   8.09µs   9.21µs   104783.        0B    10.5 
#> 11 best                 3         100   4.74µs   5.88µs   160959.        0B    32.2 
#> 12 worst                3         100   5.09µs    6.1µs   155868.        0B    31.2 
#> 13 best                 5         100   4.78µs   5.88µs   160500.        0B    32.1 
#> 14 worst                5         100   5.14µs   6.11µs   155564.        0B    31.1 
#> 15 best                10         100   4.77µs   5.84µs   159922.        0B    16.0 
#> 16 worst               10         100   5.43µs   6.47µs   145284.        0B    29.1 
#> 17 best                50         100   4.84µs   5.93µs   158707.        0B    31.7 
#> 18 worst               50         100   9.53µs  10.63µs    90446.        0B     9.05
#> 19 best               100         100   5.16µs   6.26µs   151141.        0B    15.1 
#> 20 worst              100         100  14.84µs  16.01µs    60650.        0B    12.1
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
#>  1 best                 3          15   6.48µs   8.05µs   118082.        0B     35.4
#>  2 worst                3          15   6.82µs   8.45µs   113496.        0B     22.7
#>  3 best                 5          15   6.53µs   8.06µs   116638.        0B     35.0
#>  4 worst                5          15   6.98µs   8.49µs   112623.        0B     22.5
#>  5 best                10          15   6.55µs   8.13µs   117219.        0B     23.4
#>  6 worst               10          15   7.32µs   8.99µs   104590.        0B     31.4
#>  7 best                50          15   6.86µs   8.48µs   110729.        0B     33.2
#>  8 worst               50          15  10.13µs  11.77µs    81373.        0B     16.3
#>  9 best               100          15   7.25µs    8.7µs   108864.        0B     32.7
#> 10 worst              100          15  13.56µs  15.25µs    63622.        0B     12.7
#> 11 best                 3         100   6.68µs   8.33µs   112988.        0B     33.9
#> 12 worst                3         100   7.45µs   9.14µs   102614.        0B     20.5
#> 13 best                 5         100   6.56µs   8.12µs   117791.        0B     23.6
#> 14 worst                5         100   7.52µs   9.06µs   105267.        0B     31.6
#> 15 best                10         100   6.58µs   8.03µs   118423.        0B     23.7
#> 16 worst               10         100   8.64µs  10.21µs    93626.        0B     28.1
#> 17 best                50         100   7.06µs   8.21µs   115476.        0B     23.1
#> 18 worst               50         100  15.42µs  16.04µs    60796.        0B     18.2
#> 19 best               100         100   7.36µs   7.92µs   121710.        0B     24.3
#> 20 worst              100         100  24.54µs  25.23µs    38934.        0B     11.7
```
