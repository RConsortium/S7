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
#> 1 foo_S7(x)    6.37µs   7.05µs   134061.    10.9KB     26.8
#> 2 foo_S3(x)     2.6µs   2.88µs   318755.        0B     31.9
#> 3 foo_S4(x)    2.77µs    3.1µs   312097.        0B     31.2

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
#> 1 bar_S7(x, y)  13.21µs  14.37µs    67195.        0B     26.9
#> 2 bar_S4(x, y)   7.41µs   8.24µs   118063.        0B     11.8
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
#>  1 best                 3          15   6.27µs   6.98µs   138509.        0B    27.7 
#>  2 worst                3          15    6.5µs   7.25µs   132814.        0B    26.6 
#>  3 best                 5          15    6.3µs   6.99µs   138124.        0B    27.6 
#>  4 worst                5          15   6.58µs   7.34µs   131439.        0B    26.3 
#>  5 best                10          15   6.44µs   7.11µs   136386.        0B    27.3 
#>  6 worst               10          15   6.85µs    7.6µs   126760.        0B    12.7 
#>  7 best                50          15   6.48µs   7.19µs   134795.        0B    27.0 
#>  8 worst               50          15   8.53µs   9.35µs   103653.        0B    20.7 
#>  9 best               100          15   6.65µs   7.44µs   129868.        0B    26.0 
#> 10 worst              100          15  11.02µs  11.91µs    81690.        0B    16.3 
#> 11 best                 3         100   6.44µs   7.15µs   134779.        0B    27.0 
#> 12 worst                3         100   6.79µs   7.58µs   126910.        0B    25.4 
#> 13 best                 5         100   6.52µs   7.26µs   132445.        0B    26.5 
#> 14 worst                5         100   6.99µs   7.68µs   125944.        0B    25.2 
#> 15 best                10         100   6.45µs   7.16µs   134122.        0B    26.8 
#> 16 worst               10         100   7.26µs   7.93µs   122306.        0B    12.2 
#> 17 best                50         100   6.46µs   7.21µs   133258.        0B    26.7 
#> 18 worst               50         100  12.05µs  12.81µs    75639.        0B    15.1 
#> 19 best               100         100   6.87µs   7.67µs   124886.        0B    25.0 
#> 20 worst              100         100  18.68µs  19.49µs    49902.        0B     9.98
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
#>  1 best                 3          15   8.82µs   9.72µs    98731.        0B    29.6 
#>  2 worst                3          15   9.06µs  10.05µs    96303.        0B    19.3 
#>  3 best                 5          15   8.71µs   9.69µs    98742.        0B    29.6 
#>  4 worst                5          15   9.24µs  10.16µs    94138.        0B    28.2 
#>  5 best                10          15   8.71µs   9.75µs    97388.        0B    29.2 
#>  6 worst               10          15   9.64µs   10.6µs    90988.        0B    18.2 
#>  7 best                50          15    9.1µs  10.14µs    93380.        0B    28.0 
#>  8 worst               50          15   12.9µs  13.99µs    69263.        0B    13.9 
#>  9 best               100          15   9.56µs  10.55µs    90828.        0B    27.3 
#> 10 worst              100          15  17.53µs  18.61µs    51796.        0B    15.5 
#> 11 best                 3         100   8.89µs   9.95µs    96764.        0B    19.4 
#> 12 worst                3         100   9.67µs  10.83µs    88046.        0B    26.4 
#> 13 best                 5         100   8.78µs   9.77µs    97533.        0B    29.3 
#> 14 worst                5         100   9.75µs  10.79µs    85735.        0B    25.7 
#> 15 best                10         100   8.67µs   9.49µs   101433.        0B    20.3 
#> 16 worst               10         100  10.93µs  11.48µs    84692.        0B    25.4 
#> 17 best                50         100   9.17µs   9.79µs    98902.        0B    19.8 
#> 18 worst               50         100  19.24µs  19.82µs    49148.        0B    14.7 
#> 19 best               100         100   9.62µs  10.15µs    95797.        0B    19.2 
#> 20 worst              100         100  29.95µs   30.7µs    31623.        0B     9.49
```
