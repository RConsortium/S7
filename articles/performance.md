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
#> 1 foo_S7(x)    7.03µs   7.95µs   119174.    10.9KB     23.8
#> 2 foo_S3(x)    2.69µs   3.02µs   300697.        0B     30.1
#> 3 foo_S4(x)    2.73µs   3.14µs   307129.        0B     30.7

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
#> 1 bar_S7(x, y)  14.75µs  16.07µs    59916.        0B     24.0
#> 2 bar_S4(x, y)   7.44µs   8.42µs   115214.        0B     23.0
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
#>  1 best                 3          15   6.54µs   7.46µs   129963.        0B    26.0 
#>  2 worst                3          15   6.65µs   7.52µs   128133.        0B    25.6 
#>  3 best                 5          15   6.67µs   7.51µs   128534.        0B    25.7 
#>  4 worst                5          15   6.81µs   7.69µs   125465.        0B    25.1 
#>  5 best                10          15   6.51µs   7.38µs   129086.        0B    25.8 
#>  6 worst               10          15   7.02µs   7.85µs   122330.        0B    24.5 
#>  7 best                50          15   6.82µs   7.67µs   126181.        0B    25.2 
#>  8 worst               50          15   8.92µs   9.79µs    98903.        0B    19.8 
#>  9 best               100          15   7.03µs   7.96µs   120370.        0B    24.1 
#> 10 worst              100          15  11.36µs  12.29µs    78717.        0B    15.7 
#> 11 best                 3         100   6.64µs   7.56µs   126811.        0B    25.4 
#> 12 worst                3         100   7.12µs   7.92µs   120578.        0B    24.1 
#> 13 best                 5         100   6.58µs   7.51µs   126772.        0B    25.4 
#> 14 worst                5         100   7.08µs   7.95µs   119546.        0B    23.9 
#> 15 best                10         100   6.56µs   7.53µs   126622.        0B    25.3 
#> 16 worst               10         100   7.55µs   8.42µs   113814.        0B    22.8 
#> 17 best                50         100   6.69µs   7.55µs   125287.        0B    25.1 
#> 18 worst               50         100   13.1µs  14.05µs    68965.        0B    13.8 
#> 19 best               100         100    7.2µs   8.03µs   119246.        0B    23.9 
#> 20 worst              100         100  20.82µs  21.75µs    44590.        0B     8.92
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
#>  1 best                 3          15    9.2µs   10.3µs    91564.        0B    27.5 
#>  2 worst                3          15   9.55µs  10.72µs    87957.        0B    26.4 
#>  3 best                 5          15   9.22µs  10.36µs    91290.        0B    27.4 
#>  4 worst                5          15   9.73µs  10.88µs    87008.        0B    26.1 
#>  5 best                10          15    9.4µs  10.69µs    88800.        0B    26.6 
#>  6 worst               10          15  10.21µs  11.39µs    84069.        0B    16.8 
#>  7 best                50          15   9.78µs  10.96µs    86876.        0B    26.1 
#>  8 worst               50          15  13.93µs  15.17µs    63088.        0B    18.9 
#>  9 best               100          15  10.25µs  11.53µs    82863.        0B    16.6 
#> 10 worst              100          15  19.02µs   20.3µs    47415.        0B    14.2 
#> 11 best                 3         100   9.46µs  10.67µs    88805.        0B    26.6 
#> 12 worst                3         100  10.32µs  11.67µs    81003.        0B    24.3 
#> 13 best                 5         100   9.21µs  10.55µs    89656.        0B    26.9 
#> 14 worst                5         100  10.21µs  11.23µs    85407.        0B    25.6 
#> 15 best                10         100   9.18µs   9.87µs    98349.        0B    29.5 
#> 16 worst               10         100  11.77µs   12.5µs    77613.        0B    23.3 
#> 17 best                50         100   9.87µs  10.54µs    91978.        0B    27.6 
#> 18 worst               50         100  21.62µs  22.33µs    43641.        0B    13.1 
#> 19 best               100         100  10.44µs  11.11µs    87569.        0B    26.3 
#> 20 worst              100         100  34.17µs  35.08µs    27497.        0B     8.25
```
