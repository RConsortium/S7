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
#> 1 foo_S7(x)    6.31µs   7.34µs   125432.    10.9KB     25.1
#> 2 foo_S3(x)     2.6µs   2.94µs   311170.        0B     31.1
#> 3 foo_S4(x)    2.73µs   3.13µs   297930.        0B     29.8

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
#> 1 bar_S7(x, y)  13.59µs  15.32µs    62620.        0B     25.1
#> 2 bar_S4(x, y)   7.38µs   8.45µs   115343.        0B     23.1
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
#>  1 best                 3          15   6.44µs   7.38µs   131284.        0B    26.3 
#>  2 worst                3          15    6.8µs    7.7µs   125508.        0B    25.1 
#>  3 best                 5          15   6.53µs   7.43µs   130124.        0B    26.0 
#>  4 worst                5          15   6.83µs   7.79µs   123754.        0B    24.8 
#>  5 best                10          15    6.6µs   7.59µs   126947.        0B    25.4 
#>  6 worst               10          15   7.21µs   8.11µs   118474.        0B    23.7 
#>  7 best                50          15   6.92µs   7.87µs   122742.        0B    24.6 
#>  8 worst               50          15   8.95µs   9.97µs    97058.        0B    19.4 
#>  9 best               100          15   7.13µs   8.21µs   117788.        0B    23.6 
#> 10 worst              100          15  11.49µs  12.58µs    77138.        0B    15.4 
#> 11 best                 3         100   6.71µs   7.75µs   123167.        0B    24.6 
#> 12 worst                3         100   7.25µs   8.24µs   115646.        0B    23.1 
#> 13 best                 5         100   6.81µs   7.78µs   122483.        0B    24.5 
#> 14 worst                5         100   7.22µs   8.21µs   115978.        0B    23.2 
#> 15 best                10         100   6.85µs   7.81µs   122174.        0B    24.4 
#> 16 worst               10         100   7.69µs   8.76µs   109504.        0B    21.9 
#> 17 best                50         100   6.85µs   7.99µs   116217.        0B    23.2 
#> 18 worst               50         100  13.46µs  14.39µs    67289.        0B    13.5 
#> 19 best               100         100   7.37µs   8.29µs   114627.        0B    22.9 
#> 20 worst              100         100     21µs  22.13µs    43745.        0B     8.75
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
#>  1 best                 3          15   9.39µs  10.58µs    89243.        0B    26.8 
#>  2 worst                3          15    9.8µs  11.08µs    84972.        0B    25.5 
#>  3 best                 5          15   9.45µs  10.75µs    87922.        0B    26.4 
#>  4 worst                5          15   9.95µs  11.26µs    83720.        0B    25.1 
#>  5 best                10          15   9.41µs  10.79µs    87299.        0B    26.2 
#>  6 worst               10          15   10.2µs  11.58µs    82382.        0B    16.5 
#>  7 best                50          15    9.7µs  11.17µs    84952.        0B    25.5 
#>  8 worst               50          15  13.92µs  15.38µs    60570.        0B    18.2 
#>  9 best               100          15  10.56µs  11.86µs    80522.        0B    16.1 
#> 10 worst              100          15  19.35µs  20.66µs    46617.        0B    14.0 
#> 11 best                 3         100   9.53µs  10.96µs    85784.        0B    25.7 
#> 12 worst                3         100  10.63µs  11.87µs    79962.        0B    24.0 
#> 13 best                 5         100   9.26µs  10.62µs    89483.        0B    26.9 
#> 14 worst                5         100  10.39µs  11.44µs    83619.        0B    25.1 
#> 15 best                10         100   8.99µs   9.74µs    99835.        0B    30.0 
#> 16 worst               10         100  11.68µs  12.48µs    75948.        0B    22.8 
#> 17 best                50         100  10.02µs  10.81µs    89633.        0B    26.9 
#> 18 worst               50         100  21.73µs  22.57µs    43249.        0B    13.0 
#> 19 best               100         100  10.25µs  11.06µs    87699.        0B    26.3 
#> 20 worst              100         100  34.44µs  35.29µs    27743.        0B     8.33
```
