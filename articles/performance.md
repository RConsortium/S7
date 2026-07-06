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
#> 1 foo_S7(x)    8.48µs   9.84µs    95631.    10.8KB     28.7
#> 2 foo_S3(x)    2.58µs   2.85µs   318860.        0B      0  
#> 3 foo_S4(x)    2.73µs   3.11µs   307477.        0B     30.8

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
#> 1 bar_S7(x, y)   15.6µs  17.41µs    55630.        0B     27.8
#> 2 bar_S4(x, y)   7.13µs   8.04µs   120424.        0B     24.1
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
#>  1 best                 3          15   8.75µs   10.1µs    95852.        0B     28.8
#>  2 worst                3          15   8.85µs   10.3µs    93948.        0B     28.2
#>  3 best                 5          15   8.58µs   10.1µs    95727.        0B     28.7
#>  4 worst                5          15      9µs   10.5µs    90809.        0B     27.3
#>  5 best                10          15   8.74µs   10.1µs    95639.        0B     38.3
#>  6 worst               10          15   9.05µs   10.4µs    92574.        0B     27.8
#>  7 best                50          15   9.26µs   10.8µs    89546.        0B     26.9
#>  8 worst               50          15  11.25µs   12.7µs    75496.        0B     30.2
#>  9 best               100          15   9.73µs   11.2µs    86356.        0B     34.6
#> 10 worst              100          15  13.81µs   15.3µs    62926.        0B     25.2
#> 11 best                 3         100   8.68µs   10.2µs    93822.        0B     37.5
#> 12 worst                3         100   9.05µs   10.6µs    89145.        0B     26.8
#> 13 best                 5         100   8.81µs   10.2µs    90628.        0B     27.2
#> 14 worst                5         100   9.12µs   10.7µs    88715.        0B     35.5
#> 15 best                10         100   8.73µs   10.3µs    91658.        0B     27.5
#> 16 worst               10         100   9.42µs     11µs    86461.        0B     25.9
#> 17 best                50         100   9.14µs   10.7µs    87763.        0B     35.1
#> 18 worst               50         100  14.63µs   16.3µs    58546.        0B     17.6
#> 19 best               100         100   9.91µs   11.6µs    82079.        0B     24.6
#> 20 worst              100         100  21.65µs   23.3µs    41051.        0B     16.4
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
#>  1 best                 3          15   10.9µs   12.9µs    73654.        0B     36.8
#>  2 worst                3          15   11.3µs   12.2µs    79030.        0B     31.6
#>  3 best                 5          15     11µs   11.9µs    81414.        0B     40.7
#>  4 worst                5          15   11.6µs   12.5µs    77851.        0B     31.2
#>  5 best                10          15   11.2µs     12µs    80534.        0B     40.3
#>  6 worst               10          15     12µs     13µs    74392.        0B     29.8
#>  7 best                50          15     12µs   13.1µs    72945.        0B     29.2
#>  8 worst               50          15   15.9µs     17µs    56784.        0B     28.4
#>  9 best               100          15   13.3µs   14.7µs    64829.        0B     25.9
#> 10 worst              100          15   21.1µs   22.8µs    41634.        0B     20.8
#> 11 best                 3         100   11.5µs     13µs    72552.        0B     29.0
#> 12 worst                3         100   12.2µs   13.8µs    67972.        0B     34.0
#> 13 best                 5         100   11.3µs   12.7µs    73815.        0B     29.5
#> 14 worst                5         100   12.3µs   13.8µs    69129.        0B     34.6
#> 15 best                10         100   11.3µs   12.8µs    74084.        0B     29.6
#> 16 worst               10         100   13.6µs   15.2µs    62834.        0B     31.4
#> 17 best                50         100   12.6µs   14.1µs    67288.        0B     26.9
#> 18 worst               50         100   22.6µs   24.3µs    39407.        0B     19.7
#> 19 best               100         100   13.7µs   15.3µs    61827.        0B     37.1
#> 20 worst              100         100   34.2µs   36.2µs    26582.        0B     16.0
```
