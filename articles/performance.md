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
#> 1 foo_S7(x)       5µs   6.05µs   149775.    10.9KB     30.0
#> 2 foo_S3(x)    1.95µs    2.4µs   353885.        0B     35.4
#> 3 foo_S4(x)     2.1µs   2.69µs   344981.        0B     34.5

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
#> 1 bar_S7(x, y)  10.45µs  12.44µs    77289.        0B     30.9
#> 2 bar_S4(x, y)   5.64µs   6.84µs   139169.        0B     27.8
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
#>  1 best                 3          15      5µs   6.13µs   153737.        0B    30.8 
#>  2 worst                3          15   5.23µs   6.35µs   148843.        0B    29.8 
#>  3 best                 5          15   5.04µs   6.15µs   153262.        0B    30.7 
#>  4 worst                5          15    5.3µs   6.46µs   145072.        0B    29.0 
#>  5 best                10          15   5.03µs   6.22µs   151208.        0B    30.2 
#>  6 worst               10          15   5.45µs   6.67µs   142091.        0B    28.4 
#>  7 best                50          15    5.2µs   6.41µs   147808.        0B    29.6 
#>  8 worst               50          15   7.04µs   8.29µs   114771.        0B    23.0 
#>  9 best               100          15   5.42µs    6.7µs   141012.        0B    28.2 
#> 10 worst              100          15   9.01µs  10.46µs    91392.        0B    18.3 
#> 11 best                 3         100   5.18µs   6.47µs   145304.        0B    29.1 
#> 12 worst                3         100   5.61µs   6.82µs   136714.        0B    27.3 
#> 13 best                 5         100    5.2µs    6.4µs   146122.        0B    29.2 
#> 14 worst                5         100   5.58µs   6.87µs   136589.        0B    27.3 
#> 15 best                10         100    5.1µs   6.43µs   144235.        0B    28.9 
#> 16 worst               10         100   6.08µs   7.35µs   125568.        0B    25.1 
#> 17 best                50         100   5.21µs   6.49µs   142817.        0B    28.6 
#> 18 worst               50         100   11.8µs   13.2µs    72547.        0B    14.5 
#> 19 best               100         100    5.6µs   6.87µs   135276.        0B    27.1 
#> 20 worst              100         100  18.98µs   20.9µs    46587.        0B     9.32
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
#>  1 best                 3          15   7.01µs   8.97µs   103972.        0B    31.2 
#>  2 worst                3          15   7.36µs   9.38µs    99946.        0B    30.0 
#>  3 best                 5          15   7.14µs   8.96µs   104276.        0B    31.3 
#>  4 worst                5          15   7.45µs   9.53µs    98282.        0B    29.5 
#>  5 best                10          15   7.19µs   9.09µs   102981.        0B    30.9 
#>  6 worst               10          15   8.08µs     10µs    94111.        0B    18.8 
#>  7 best                50          15    7.5µs   9.46µs    98630.        0B    29.6 
#>  8 worst               50          15  10.92µs  12.94µs    73428.        0B    22.0 
#>  9 best               100          15   8.04µs   9.95µs    93954.        0B    28.2 
#> 10 worst              100          15   15.3µs  17.32µs    55373.        0B    16.6 
#> 11 best                 3         100    7.5µs   9.38µs    99078.        0B    29.7 
#> 12 worst                3         100   8.61µs  10.49µs    89932.        0B    27.0 
#> 13 best                 5         100   7.24µs    9.1µs   101970.        0B    30.6 
#> 14 worst                5         100   8.45µs   9.52µs    97998.        0B    29.4 
#> 15 best                10         100   7.23µs   7.88µs   119758.        0B    35.9 
#> 16 worst               10         100   10.1µs  10.81µs    89050.        0B    26.7 
#> 17 best                50         100   7.88µs   8.54µs   111827.        0B    33.6 
#> 18 worst               50         100  19.28µs  20.46µs    47737.        0B    14.3 
#> 19 best               100         100   7.93µs   8.81µs   108731.        0B    32.6 
#> 20 worst              100         100  31.84µs  33.53µs    29250.        0B     8.78
```
