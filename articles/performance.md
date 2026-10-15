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
#> 1 foo_S7(x)    6.62µs   7.36µs   128404.    10.9KB     25.7
#> 2 foo_S3(x)    2.61µs    2.9µs   314451.        0B     31.4
#> 3 foo_S4(x)    2.77µs   3.17µs   302553.        0B     30.3

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
#> 1 bar_S7(x, y)  13.86µs  15.25µs    62874.        0B     25.2
#> 2 bar_S4(x, y)   7.21µs   8.37µs   115468.        0B     23.1
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
#>  1 best                 3          15   6.57µs   7.46µs   129918.        0B    26.0 
#>  2 worst                3          15   6.98µs   7.76µs   124598.        0B    24.9 
#>  3 best                 5          15   6.77µs   7.55µs   127326.        0B    25.5 
#>  4 worst                5          15   7.07µs   7.88µs   122436.        0B    24.5 
#>  5 best                10          15   6.77µs   7.62µs   127049.        0B    25.4 
#>  6 worst               10          15   7.21µs   8.08µs   119161.        0B    23.8 
#>  7 best                50          15   6.94µs    7.8µs   123509.        0B    24.7 
#>  8 worst               50          15   9.06µs   9.86µs    98319.        0B    19.7 
#>  9 best               100          15   7.25µs   8.07µs   119882.        0B    24.0 
#> 10 worst              100          15  11.27µs  12.16µs    79835.        0B    16.0 
#> 11 best                 3         100   6.91µs   7.72µs   124142.        0B    24.8 
#> 12 worst                3         100   7.26µs   8.15µs   117243.        0B    23.5 
#> 13 best                 5         100   6.89µs   7.79µs   122739.        0B    24.6 
#> 14 worst                5         100   7.34µs   8.18µs   117308.        0B    23.5 
#> 15 best                10         100   6.77µs   7.67µs   123617.        0B    24.7 
#> 16 worst               10         100   7.66µs    8.5µs   109432.        0B    21.9 
#> 17 best                50         100   6.91µs   7.75µs   123446.        0B    24.7 
#> 18 worst               50         100  12.57µs   13.4µs    72208.        0B    14.4 
#> 19 best               100         100   7.31µs    8.2µs   116292.        0B    23.3 
#> 20 worst              100         100  18.93µs  19.94µs    48564.        0B     9.71
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
#>  1 best                 3          15   9.43µs   10.6µs    89227.        0B    26.8 
#>  2 worst                3          15   9.92µs   11.1µs    84942.        0B    25.5 
#>  3 best                 5          15   9.62µs   10.8µs    88226.        0B    26.5 
#>  4 worst                5          15  10.14µs   11.4µs    83556.        0B    25.1 
#>  5 best                10          15   9.58µs   10.8µs    88025.        0B    26.4 
#>  6 worst               10          15  10.52µs   11.8µs    80226.        0B    24.1 
#>  7 best                50          15   9.84µs   11.4µs    76169.        0B    22.9 
#>  8 worst               50          15  13.86µs   15.1µs    63000.        0B    18.9 
#>  9 best               100          15  10.47µs   11.8µs    81642.        0B    16.3 
#> 10 worst              100          15  18.48µs     20µs    47946.        0B    14.4 
#> 11 best                 3         100   9.83µs     11µs    86518.        0B    17.3 
#> 12 worst                3         100  10.66µs   11.8µs    80180.        0B    24.1 
#> 13 best                 5         100   9.64µs   10.9µs    86811.        0B    26.1 
#> 14 worst                5         100   10.4µs   11.4µs    84615.        0B    25.4 
#> 15 best                10         100   9.44µs   10.1µs    95801.        0B    28.7 
#> 16 worst               10         100  11.85µs   12.6µs    76997.        0B    23.1 
#> 17 best                50         100  10.03µs   10.7µs    90949.        0B    27.3 
#> 18 worst               50         100  20.12µs   20.9µs    46572.        0B    14.0 
#> 19 best               100         100  10.39µs   11.1µs    87278.        0B    17.5 
#> 20 worst              100         100  30.96µs   31.8µs    30666.        0B     9.20
```
