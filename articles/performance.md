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
#> 1 foo_S7(x)    7.94µs   9.46µs   100265.    10.8KB     30.1
#> 2 foo_S3(x)    2.49µs   2.89µs   316307.        0B      0  
#> 3 foo_S4(x)    2.68µs   3.17µs   301079.        0B     30.1

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
#> 1 bar_S7(x, y)   14.6µs  16.77µs    57718.        0B     23.1
#> 2 bar_S4(x, y)   6.91µs   7.94µs   122218.        0B     24.4
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
#>  1 best                 3          15   8.15µs   9.69µs    99007.        0B     29.7
#>  2 worst                3          15   8.42µs   9.79µs    98939.        0B     29.7
#>  3 best                 5          15   8.24µs   9.68µs   100404.        0B     30.1
#>  4 worst                5          15    8.4µs    9.8µs    98601.        0B     29.6
#>  5 best                10          15   8.35µs   9.94µs    97583.        0B     29.3
#>  6 worst               10          15   8.57µs  10.04µs    96711.        0B     29.0
#>  7 best                50          15   8.67µs  10.14µs    95554.        0B     38.2
#>  8 worst               50          15  10.36µs   11.9µs    81735.        0B     24.5
#>  9 best               100          15   9.28µs  10.77µs    90317.        0B     36.1
#> 10 worst              100          15  12.51µs  14.12µs    69053.        0B     27.6
#> 11 best                 3         100   8.26µs   9.94µs    96880.        0B     29.1
#> 12 worst                3         100   8.46µs  10.34µs    92704.        0B     37.1
#> 13 best                 5         100   8.36µs  10.14µs    94353.        0B     28.3
#> 14 worst                5         100   8.67µs   10.4µs    92040.        0B     27.6
#> 15 best                10         100   8.25µs  10.04µs    95429.        0B     28.6
#> 16 worst               10         100   8.99µs  10.68µs    89970.        0B     27.0
#> 17 best                50         100   8.71µs  10.41µs    91264.        0B     36.5
#> 18 worst               50         100  13.82µs  15.49µs    62009.        0B     18.6
#> 19 best               100         100   9.43µs   10.8µs    89024.        0B     35.6
#> 20 worst              100         100  20.05µs  21.63µs    44909.        0B     18.0
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
#>  1 best                 3          15   10.4µs   11.7µs    82315.        0B     41.2
#>  2 worst                3          15   10.8µs   11.5µs    85048.        0B     34.0
#>  3 best                 5          15   10.5µs   11.3µs    86657.        0B     43.4
#>  4 worst                5          15     11µs   11.7µs    83828.        0B     33.5
#>  5 best                10          15   10.9µs   11.6µs    84508.        0B     42.3
#>  6 worst               10          15   11.6µs   12.5µs    77876.        0B     31.2
#>  7 best                50          15   11.9µs   12.9µs    75417.        0B     30.2
#>  8 worst               50          15   15.1µs   16.2µs    60204.        0B     30.1
#>  9 best               100          15     13µs   14.9µs    63586.        0B     31.8
#> 10 worst              100          15   19.7µs   21.4µs    45047.        0B     18.0
#> 11 best                 3         100     11µs   12.4µs    76482.        0B     30.6
#> 12 worst                3         100   11.9µs   13.3µs    71469.        0B     35.8
#> 13 best                 5         100   10.8µs   12.2µs    77818.        0B     38.9
#> 14 worst                5         100   11.8µs   13.1µs    73016.        0B     29.2
#> 15 best                10         100   10.8µs   12.2µs    78770.        0B     31.5
#> 16 worst               10         100     13µs   14.5µs    66160.        0B     33.1
#> 17 best                50         100   12.1µs   13.8µs    68352.        0B     34.2
#> 18 worst               50         100   21.6µs   23.4µs    41225.        0B     16.5
#> 19 best               100         100   13.1µs   14.6µs    65305.        0B     39.2
#> 20 worst              100         100   32.5µs   35.1µs    26543.        0B     13.3
```
