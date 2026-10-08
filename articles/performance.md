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
#> 1 foo_S7(x)    6.41µs   7.18µs   131940.    10.9KB     26.4
#> 2 foo_S3(x)    2.58µs   2.85µs   321007.        0B     32.1
#> 3 foo_S4(x)    2.75µs    3.1µs   311148.        0B     31.1

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
#> 1 bar_S7(x, y)   13.1µs  14.22µs    67574.        0B     20.3
#> 2 bar_S4(x, y)   7.29µs   8.12µs   119250.        0B     23.9
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
#>  1 best                 3          15   6.17µs      7µs   137452.        0B     27.5
#>  2 worst                3          15   6.42µs   7.16µs   135213.        0B     27.0
#>  3 best                 5          15   6.19µs   6.98µs   137648.        0B     27.5
#>  4 worst                5          15    6.5µs   7.23µs   133512.        0B     13.4
#>  5 best                10          15   6.21µs   6.99µs   134436.        0B     26.9
#>  6 worst               10          15    6.6µs   7.42µs   130310.        0B     13.0
#>  7 best                50          15   6.46µs   7.23µs   133799.        0B     26.8
#>  8 worst               50          15   8.53µs   9.28µs   104296.        0B     20.9
#>  9 best               100          15   6.71µs   7.44µs   130311.        0B     26.1
#> 10 worst              100          15  10.76µs  11.61µs    83071.        0B     16.6
#> 11 best                 3         100   6.35µs   7.16µs   133826.        0B     26.8
#> 12 worst                3         100   6.84µs   7.66µs   125624.        0B     25.1
#> 13 best                 5         100   6.39µs   7.24µs   131952.        0B     26.4
#> 14 worst                5         100   6.88µs   7.75µs   123802.        0B     24.8
#> 15 best                10         100   6.39µs   7.27µs   131507.        0B     26.3
#> 16 worst               10         100   7.16µs   8.06µs   119836.        0B     12.0
#> 17 best                50         100   6.39µs   7.29µs   131097.        0B     26.2
#> 18 worst               50         100  11.87µs  12.78µs    75616.        0B     15.1
#> 19 best               100         100   6.71µs   7.62µs   125433.        0B     25.1
#> 20 worst              100         100  18.37µs  19.35µs    50100.        0B     10.0
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
#>  1 best                 3          15   8.51µs   9.66µs    98362.        0B    29.5 
#>  2 worst                3          15      9µs  10.13µs    92137.        0B    18.4 
#>  3 best                 5          15   8.54µs   9.64µs    97067.        0B    29.1 
#>  4 worst                5          15   9.07µs  10.07µs    94157.        0B    28.3 
#>  5 best                10          15   8.62µs   9.77µs    97000.        0B    29.1 
#>  6 worst               10          15   9.67µs  10.72µs    89197.        0B    17.8 
#>  7 best                50          15    9.1µs   10.2µs    93011.        0B    27.9 
#>  8 worst               50          15  12.92µs   14.1µs    68135.        0B    13.6 
#>  9 best               100          15   9.62µs   10.7µs    88746.        0B    26.6 
#> 10 worst              100          15  17.77µs  18.86µs    50842.        0B    15.3 
#> 11 best                 3         100   8.87µs   9.99µs    94775.        0B    28.4 
#> 12 worst                3         100   9.75µs   10.9µs    87696.        0B    17.5 
#> 13 best                 5         100   8.91µs   9.99µs    94806.        0B    28.5 
#> 14 worst                5         100   9.77µs  10.86µs    87249.        0B    26.2 
#> 15 best                10         100   8.48µs   9.52µs   100665.        0B    30.2 
#> 16 worst               10         100  10.93µs   11.6µs    83521.        0B    16.7 
#> 17 best                50         100   9.25µs   9.93µs    97127.        0B    19.4 
#> 18 worst               50         100  19.28µs  20.06µs    48641.        0B    14.6 
#> 19 best               100         100   9.72µs   10.5µs    92183.        0B    27.7 
#> 20 worst              100         100  30.36µs  31.15µs    31374.        0B     6.28
```
