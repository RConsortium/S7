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
#> 1 foo_S7(x)    7.37µs   8.24µs   113403.    18.2KB     22.7
#> 2 foo_S3(x)    2.52µs   2.73µs   332586.        0B      0  
#> 3 foo_S4(x)    2.73µs   3.03µs   319068.        0B     31.9

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
#> 1 bar_S7(x, y)   13.2µs  14.55µs    66619.        0B     20.0
#> 2 bar_S4(x, y)   7.18µs   7.77µs   125462.        0B     25.1
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
#>  1 best                 3          15   7.44µs   8.53µs   114166.        0B     22.8
#>  2 worst                3          15   7.66µs   8.61µs   113313.        0B     34.0
#>  3 best                 5          15   7.41µs   8.37µs   116362.        0B     34.9
#>  4 worst                5          15   7.68µs   8.59µs   113544.        0B     34.1
#>  5 best                10          15   7.51µs   8.42µs   115582.        0B     34.7
#>  6 worst               10          15   7.96µs   8.76µs   111291.        0B     22.3
#>  7 best                50          15   7.89µs   8.84µs   110094.        0B     33.0
#>  8 worst               50          15   9.83µs   10.8µs    90019.        0B     27.0
#>  9 best               100          15   8.61µs   9.61µs   101358.        0B     30.4
#> 10 worst              100          15  12.69µs  13.69µs    71414.        0B     21.4
#> 11 best                 3         100   7.47µs    8.4µs   115483.        0B     34.7
#> 12 worst                3         100   7.82µs   8.63µs   112742.        0B     33.8
#> 13 best                 5         100   7.52µs    8.4µs   115868.        0B     46.4
#> 14 worst                5         100   7.84µs   8.77µs   110579.        0B     22.1
#> 15 best                10         100   7.47µs   8.51µs   113367.        0B     22.7
#> 16 worst               10         100   8.23µs   9.24µs   104812.        0B     31.5
#> 17 best                50         100   7.97µs   9.06µs   106887.        0B     32.1
#> 18 worst               50         100  13.51µs  14.58µs    66829.        0B     20.1
#> 19 best               100         100   8.89µs   9.95µs    97601.        0B     29.3
#> 20 worst              100         100  20.42µs  21.53µs    45236.        0B     13.6
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
#>  1 best                 3          15   9.01µs   10.3µs    93088.        0B     27.9
#>  2 worst                3          15   9.31µs   10.4µs    93132.        0B     37.3
#>  3 best                 5          15   9.03µs   10.1µs    95611.        0B     28.7
#>  4 worst                5          15   9.54µs   10.7µs    91209.        0B     36.5
#>  5 best                10          15   9.24µs   10.3µs    94060.        0B     37.6
#>  6 worst               10          15   9.98µs   10.8µs    89829.        0B     27.0
#>  7 best                50          15  10.13µs   10.7µs    90730.        0B     36.3
#>  8 worst               50          15  13.82µs   14.5µs    67262.        0B     20.2
#>  9 best               100          15  11.37µs   11.9µs    81984.        0B     32.8
#> 10 worst              100          15  19.31µs   20.1µs    48470.        0B     19.4
#> 11 best                 3         100   9.22µs   10.1µs    95373.        0B     38.2
#> 12 worst                3         100  10.07µs   10.9µs    88412.        0B     26.5
#> 13 best                 5         100   9.09µs   10.1µs    95769.        0B     38.3
#> 14 worst                5         100  10.14µs   11.1µs    87290.        0B     26.2
#> 15 best                10         100   9.13µs   10.1µs    94334.        0B     37.7
#> 16 worst               10         100  11.43µs   12.3µs    79362.        0B     23.8
#> 17 best                50         100  10.35µs   11.3µs    85813.        0B     34.3
#> 18 worst               50         100  20.37µs   21.3µs    45744.        0B     13.7
#> 19 best               100         100   11.6µs   12.5µs    77717.        0B     31.1
#> 20 worst              100         100  32.13µs   33.2µs    29372.        0B     11.8
```
