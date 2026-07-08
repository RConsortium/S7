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
#> 1 foo_S7(x)    8.77µs   10.1µs    93881.    10.8KB     28.2
#> 2 foo_S3(x)    2.58µs   2.86µs   316428.        0B      0  
#> 3 foo_S4(x)    2.77µs   3.16µs   303992.        0B     30.4

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
#> 1 bar_S7(x, y)  16.06µs  17.75µs    54173.        0B     21.7
#> 2 bar_S4(x, y)   7.22µs   8.12µs   118450.        0B     23.7
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
#>  1 best                 3          15   8.53µs   9.85µs    98484.        0B     29.6
#>  2 worst                3          15   8.81µs  10.04µs    96418.        0B     28.9
#>  3 best                 5          15    8.8µs  10.14µs    95748.        0B     28.7
#>  4 worst                5          15   8.97µs  10.27µs    94320.        0B     28.3
#>  5 best                10          15   8.76µs   9.89µs    97834.        0B     29.4
#>  6 worst               10          15   9.28µs  10.31µs    93996.        0B     28.2
#>  7 best                50          15   9.25µs  10.41µs    92762.        0B     37.1
#>  8 worst               50          15  11.39µs  12.62µs    76704.        0B     23.0
#>  9 best               100          15    9.9µs  11.21µs    86248.        0B     34.5
#> 10 worst              100          15  13.93µs  15.22µs    63710.        0B     25.5
#> 11 best                 3         100   8.95µs  10.44µs    91575.        0B     27.5
#> 12 worst                3         100   9.25µs  10.83µs    88498.        0B     35.4
#> 13 best                 5         100   8.96µs  10.58µs    90670.        0B     27.2
#> 14 worst                5         100   9.33µs  10.89µs    86272.        0B     25.9
#> 15 best                10         100   8.87µs  10.51µs    91584.        0B     27.5
#> 16 worst               10         100   9.72µs  11.19µs    86530.        0B     26.0
#> 17 best                50         100   9.23µs  10.54µs    91497.        0B     27.5
#> 18 worst               50         100  14.91µs  16.15µs    59960.        0B     18.0
#> 19 best               100         100   9.93µs  11.42µs    84423.        0B     33.8
#> 20 worst              100         100  21.82µs   23.3µs    41567.        0B     16.6
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
#>  1 best                 3          15   11.2µs   12.5µs    76837.        0B     38.4
#>  2 worst                3          15   11.6µs   12.5µs    77497.        0B     31.0
#>  3 best                 5          15   11.4µs   12.2µs    79504.        0B     39.8
#>  4 worst                5          15   11.9µs   12.8µs    76453.        0B     30.6
#>  5 best                10          15   11.5µs   12.2µs    79399.        0B     39.7
#>  6 worst               10          15   12.2µs   13.3µs    73073.        0B     29.2
#>  7 best                50          15   12.3µs   13.4µs    71998.        0B     28.8
#>  8 worst               50          15   16.2µs   17.4µs    55981.        0B     28.0
#>  9 best               100          15   13.5µs   14.9µs    63715.        0B     31.9
#> 10 worst              100          15   21.6µs     23µs    41590.        0B     16.6
#> 11 best                 3         100   11.7µs   13.2µs    71184.        0B     35.6
#> 12 worst                3         100   12.6µs   13.9µs    68510.        0B     34.3
#> 13 best                 5         100   11.6µs   12.9µs    74214.        0B     29.7
#> 14 worst                5         100   12.7µs   13.9µs    68222.        0B     34.1
#> 15 best                10         100   11.8µs     13µs    73472.        0B     36.8
#> 16 worst               10         100   13.8µs   15.1µs    63272.        0B     25.3
#> 17 best                50         100   12.9µs   14.2µs    66774.        0B     33.4
#> 18 worst               50         100   23.1µs   24.5µs    39035.        0B     19.5
#> 19 best               100         100   13.7µs   15.1µs    62530.        0B     37.5
#> 20 worst              100         100   34.5µs   36.7µs    26043.        0B     13.0
```
