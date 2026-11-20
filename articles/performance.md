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
#> 1 foo_S7(x)    4.79µs   5.59µs   169183.    10.9KB     33.8
#> 2 foo_S3(x)    1.97µs   2.31µs   394165.        0B     39.4
#> 3 foo_S4(x)    2.07µs   2.49µs   383949.        0B     38.4

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
#> 1 bar_S7(x, y)  10.04µs  11.35µs    85411.        0B     34.2
#> 2 bar_S4(x, y)   5.55µs   6.41µs   151910.        0B     30.4
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
#>  1 best                 3          15   4.79µs   5.66µs   170132.        0B     34.0
#>  2 worst                3          15   4.96µs   5.79µs   167072.        0B     33.4
#>  3 best                 5          15   4.87µs   5.72µs   168829.        0B     33.8
#>  4 worst                5          15   5.03µs   5.85µs   165566.        0B     33.1
#>  5 best                10          15   4.84µs   5.74µs   165487.        0B     33.1
#>  6 worst               10          15   5.17µs   6.02µs   160805.        0B     32.2
#>  7 best                50          15   5.05µs   5.97µs   161615.        0B     32.3
#>  8 worst               50          15   6.51µs   7.48µs   129968.        0B     26.0
#>  9 best               100          15   5.27µs   6.15µs   157702.        0B     31.5
#> 10 worst              100          15   8.29µs   9.12µs   107215.        0B     21.4
#> 11 best                 3         100   4.94µs   5.82µs   165602.        0B     33.1
#> 12 worst                3         100   5.29µs   6.06µs   158844.        0B     31.8
#> 13 best                 5         100      5µs    5.8µs   164419.        0B     32.9
#> 14 worst                5         100    5.3µs   6.11µs   157418.        0B     31.5
#> 15 best                10         100   4.92µs   5.74µs   165258.        0B     33.1
#> 16 worst               10         100   5.64µs   6.43µs   149555.        0B     29.9
#> 17 best                50         100   5.01µs   5.81µs   165584.        0B     33.1
#> 18 worst               50         100   9.98µs  10.82µs    89898.        0B     18.0
#> 19 best               100         100    5.4µs   6.19µs   155681.        0B     31.1
#> 20 worst              100         100  15.86µs  16.84µs    58156.        0B     11.6
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
#>  1 best                 3          15    6.8µs   7.97µs   118762.        0B     35.6
#>  2 worst                3          15   6.97µs   8.08µs   118219.        0B     35.5
#>  3 best                 5          15   6.76µs   7.82µs   120530.        0B     36.2
#>  4 worst                5          15   7.07µs   8.07µs   118925.        0B     35.7
#>  5 best                10          15   6.86µs   7.89µs   121281.        0B     36.4
#>  6 worst               10          15   7.46µs   8.51µs   113124.        0B     22.6
#>  7 best                50          15   7.14µs   8.25µs   115815.        0B     34.8
#>  8 worst               50          15  10.17µs  11.37µs    84781.        0B     25.4
#>  9 best               100          15   7.62µs   8.88µs   107713.        0B     21.5
#> 10 worst              100          15   13.8µs  15.05µs    64229.        0B     19.3
#> 11 best                 3         100   7.04µs   8.17µs   115711.        0B     34.7
#> 12 worst                3         100   7.72µs   8.76µs   109845.        0B     33.0
#> 13 best                 5         100   6.82µs   7.94µs   120225.        0B     36.1
#> 14 worst                5         100   7.63µs   8.48µs   113680.        0B     34.1
#> 15 best                10         100   6.78µs    7.4µs   126585.        0B     38.0
#> 16 worst               10         100   8.88µs    9.5µs   102686.        0B     30.8
#> 17 best                50         100   7.42µs   7.98µs   121516.        0B     36.5
#> 18 worst               50         100  16.57µs  17.24µs    56942.        0B     17.1
#> 19 best               100         100   7.78µs   8.32µs   116878.        0B     35.1
#> 20 worst              100         100  26.58µs  27.28µs    36037.        0B     10.8
```
