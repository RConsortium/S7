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
#> 1 foo_S7(x)    8.11µs   9.32µs   100835.    10.8KB     30.3
#> 2 foo_S3(x)    2.52µs   2.77µs   327206.        0B     32.7
#> 3 foo_S4(x)    2.71µs   3.06µs   313719.        0B     31.4

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
#> 1 bar_S7(x, y)  14.95µs  16.59µs    58309.        0B     29.2
#> 2 bar_S4(x, y)   7.12µs   8.01µs   121538.        0B     24.3
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
#>  1 best                 3          15   7.99µs   9.38µs   103475.        0B     31.1
#>  2 worst                3          15   8.39µs   9.73µs    99683.        0B     29.9
#>  3 best                 5          15   8.29µs   9.61µs   100929.        0B     30.3
#>  4 worst                5          15   8.42µs   9.76µs    99043.        0B     29.7
#>  5 best                10          15   8.15µs   9.48µs   101594.        0B     30.5
#>  6 worst               10          15   8.68µs   9.92µs    97619.        0B     29.3
#>  7 best                50          15   8.29µs   9.64µs   100508.        0B     30.2
#>  8 worst               50          15  10.29µs  11.68µs    82605.        0B     24.8
#>  9 best               100          15    8.7µs  10.11µs    94633.        0B     28.4
#> 10 worst              100          15  12.68µs  14.21µs    68228.        0B     20.5
#> 11 best                 3         100   8.28µs   9.64µs    98034.        0B     39.2
#> 12 worst                3         100   8.69µs  10.02µs    96673.        0B     29.0
#> 13 best                 5         100   8.38µs   9.67µs    99703.        0B     29.9
#> 14 worst                5         100   8.73µs  10.11µs    95173.        0B     28.6
#> 15 best                10         100   8.34µs   9.63µs    99590.        0B     39.9
#> 16 worst               10         100   9.02µs  10.43µs    92277.        0B     27.7
#> 17 best                50         100   8.36µs   9.59µs   100404.        0B     40.2
#> 18 worst               50         100  13.89µs  15.29µs    63284.        0B     19.0
#> 19 best               100         100    8.8µs  10.05µs    95602.        0B     28.7
#> 20 worst              100         100  20.53µs  22.05µs    43900.        0B     13.2
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
#>  1 best                 3          15   10.3µs   11.8µs    81924.        0B     32.8
#>  2 worst                3          15   10.8µs   12.2µs    79630.        0B     31.9
#>  3 best                 5          15   10.4µs     12µs    80208.        0B     32.1
#>  4 worst                5          15   10.7µs   12.2µs    79370.        0B     31.8
#>  5 best                10          15   10.4µs   11.2µs    87262.        0B     34.9
#>  6 worst               10          15   11.1µs   11.8µs    82364.        0B     33.0
#>  7 best                50          15   10.6µs   11.4µs    85453.        0B     34.2
#>  8 worst               50          15   14.4µs   15.3µs    63708.        0B     25.5
#>  9 best               100          15   11.1µs   12.2µs    77911.        0B     31.2
#> 10 worst              100          15   18.8µs   20.1µs    47319.        0B     18.9
#> 11 best                 3         100   10.5µs   11.7µs    80860.        0B     32.4
#> 12 worst                3         100   11.2µs   12.4µs    76922.        0B     30.8
#> 13 best                 5         100   10.4µs   11.5µs    82844.        0B     33.2
#> 14 worst                5         100   11.4µs   12.5µs    75838.        0B     30.3
#> 15 best                10         100   10.3µs   11.3µs    83961.        0B     33.6
#> 16 worst               10         100   12.7µs   13.7µs    70307.        0B     28.1
#> 17 best                50         100   11.1µs   12.2µs    77240.        0B     30.9
#> 18 worst               50         100   21.1µs   22.2µs    43449.        0B     17.4
#> 19 best               100         100   11.3µs   12.5µs    75786.        0B     30.3
#> 20 worst              100         100   31.9µs   33.3µs    28909.        0B     14.5
```
