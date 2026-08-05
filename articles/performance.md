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
#> 1 foo_S7(x)    5.98µs   7.34µs   128926.    10.8KB     38.7
#> 2 foo_S3(x)    1.95µs   2.27µs   400139.        0B     40.0
#> 3 foo_S4(x)    2.08µs   2.56µs   373846.        0B     37.4

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
#> 1 bar_S7(x, y)  10.83µs  12.76µs    76210.        0B     38.1
#> 2 bar_S4(x, y)   5.42µs   6.35µs   153710.        0B     30.7
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
#>  1 best                 3          15   5.98µs   7.54µs   128300.        0B     38.5
#>  2 worst                3          15   6.16µs   7.86µs   123155.        0B     37.0
#>  3 best                 5          15   6.07µs   7.69µs   126066.        0B     37.8
#>  4 worst                5          15   6.16µs   7.63µs   127061.        0B     38.1
#>  5 best                10          15    6.1µs   7.52µs   129322.        0B     38.8
#>  6 worst               10          15   6.27µs   7.63µs   127258.        0B     38.2
#>  7 best                50          15   6.19µs   7.66µs   126402.        0B     37.9
#>  8 worst               50          15   7.35µs   8.88µs   109189.        0B     32.8
#>  9 best               100          15   6.42µs   7.96µs   121682.        0B     36.5
#> 10 worst              100          15   8.93µs  10.51µs    92538.        0B     37.0
#> 11 best                 3         100   6.12µs   7.69µs   125860.        0B     37.8
#> 12 worst                3         100   6.45µs   7.97µs   121067.        0B     48.4
#> 13 best                 5         100   6.15µs   7.83µs   122567.        0B     36.8
#> 14 worst                5         100   6.44µs   8.02µs   119477.        0B     35.9
#> 15 best                10         100   6.22µs   8.04µs   118391.        0B     35.5
#> 16 worst               10         100   6.71µs   8.56µs   110581.        0B     44.2
#> 17 best                50         100   6.28µs   8.06µs   118009.        0B     35.4
#> 18 worst               50         100  10.32µs  12.08µs    79491.        0B     31.8
#> 19 best               100         100   6.57µs   8.21µs   116697.        0B     35.0
#> 20 worst              100         100  14.89µs  16.59µs    58675.        0B     17.6
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
#>  1 best                 3          15   7.69µs   9.55µs   100511.        0B     40.2
#>  2 worst                3          15   8.03µs   9.91µs    96686.        0B     38.7
#>  3 best                 5          15   7.63µs    9.6µs   100067.        0B     40.0
#>  4 worst                5          15    7.9µs    9.2µs   104075.        0B     41.6
#>  5 best                10          15   7.75µs    8.7µs   111959.        0B     44.8
#>  6 worst               10          15   8.26µs   9.12µs   106796.        0B     42.7
#>  7 best                50          15   8.04µs   8.87µs   110119.        0B     44.1
#>  8 worst               50          15  10.61µs  11.41µs    85956.        0B     34.4
#>  9 best               100          15   8.44µs   9.51µs   101145.        0B     40.5
#> 10 worst              100          15  13.54µs  14.76µs    65809.        0B     26.3
#> 11 best                 3         100   7.91µs   9.02µs   106609.        0B     42.7
#> 12 worst                3         100   8.57µs   9.66µs    99907.        0B     40.0
#> 13 best                 5         100   7.85µs   8.95µs   107306.        0B     42.9
#> 14 worst                5         100   8.68µs    9.8µs    97838.        0B     39.2
#> 15 best                10         100   7.86µs   9.02µs   106214.        0B     42.5
#> 16 worst               10         100   9.62µs  10.93µs    87645.        0B     35.1
#> 17 best                50         100   8.47µs   9.57µs    99958.        0B     40.0
#> 18 worst               50         100   15.6µs  16.86µs    57064.        0B     22.8
#> 19 best               100         100   8.69µs  10.01µs    94765.        0B     37.9
#> 20 worst              100         100  23.65µs  25.02µs    39007.        0B     15.6
```
