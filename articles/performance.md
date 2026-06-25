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
#> 1 foo_S7(x)    7.61µs   8.86µs   105785.    10.8KB     31.7
#> 2 foo_S3(x)    2.56µs   2.85µs   306093.        0B     30.6
#> 3 foo_S4(x)    2.71µs   3.13µs   307388.        0B     30.7

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
#> 1 bar_S7(x, y)  14.09µs  15.76µs    61238.        0B     24.5
#> 2 bar_S4(x, y)   7.45µs   8.46µs   114874.        0B     23.0
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
#>  1 best                 3          15    7.5µs   8.92µs   108229.        0B     32.5
#>  2 worst                3          15   7.86µs   9.25µs   104566.        0B     31.4
#>  3 best                 5          15   7.53µs   9.02µs   107020.        0B     32.1
#>  4 worst                5          15   7.79µs   9.26µs   104393.        0B     31.3
#>  5 best                10          15   7.58µs    9.1µs   106449.        0B     31.9
#>  6 worst               10          15      8µs    9.5µs   101778.        0B     30.5
#>  7 best                50          15   7.96µs   9.67µs   100006.        0B     30.0
#>  8 worst               50          15  10.21µs  11.71µs    82164.        0B     24.7
#>  9 best               100          15   8.54µs  10.07µs    95843.        0B     28.8
#> 10 worst              100          15  12.68µs  14.11µs    68595.        0B     20.6
#> 11 best                 3         100   7.75µs   9.19µs   103462.        0B     31.0
#> 12 worst                3         100   8.24µs   9.68µs    99735.        0B     29.9
#> 13 best                 5         100   7.54µs   9.18µs   104472.        0B     31.4
#> 14 worst                5         100    7.9µs   9.46µs   100022.        0B     30.0
#> 15 best                10         100   7.53µs   9.23µs   103464.        0B     31.0
#> 16 worst               10         100   8.34µs   9.92µs    95806.        0B     28.8
#> 17 best                50         100   8.13µs   9.65µs    98937.        0B     29.7
#> 18 worst               50         100  13.72µs  15.34µs    62800.        0B     18.8
#> 19 best               100         100   8.86µs  10.47µs    91073.        0B     27.3
#> 20 worst              100         100  20.29µs  22.15µs    43445.        0B     13.0
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
#>  1 best                 3          15   9.38µs   11.3µs    83701.        0B     33.5
#>  2 worst                3          15   9.79µs   11.7µs    80288.        0B     32.1
#>  3 best                 5          15   9.36µs   11.3µs    84184.        0B     33.7
#>  4 worst                5          15   9.77µs   11.3µs    84295.        0B     33.7
#>  5 best                10          15   9.36µs   10.1µs    95481.        0B     38.2
#>  6 worst               10          15  10.23µs     11µs    87704.        0B     26.3
#>  7 best                50          15  10.36µs   11.2µs    86756.        0B     34.7
#>  8 worst               50          15  14.07µs   14.9µs    65395.        0B     19.6
#>  9 best               100          15  11.61µs   12.8µs    74248.        0B     29.7
#> 10 worst              100          15  19.31µs   20.7µs    46557.        0B     18.6
#> 11 best                 3         100    9.6µs   10.9µs    85023.        0B     34.0
#> 12 worst                3         100  10.48µs   11.7µs    81462.        0B     32.6
#> 13 best                 5         100   9.57µs   10.8µs    87948.        0B     35.2
#> 14 worst                5         100  10.52µs     12µs    78896.        0B     31.6
#> 15 best                10         100   9.61µs     11µs    85587.        0B     34.2
#> 16 worst               10         100  11.92µs   13.3µs    71194.        0B     28.5
#> 17 best                50         100  10.84µs   12.2µs    77177.        0B     30.9
#> 18 worst               50         100  20.82µs   22.4µs    42810.        0B     17.1
#> 19 best               100         100  11.82µs   13.5µs    70071.        0B     28.0
#> 20 worst              100         100  32.41µs   34.4µs    26890.        0B     13.5
```
