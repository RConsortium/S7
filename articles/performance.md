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
#> 1 foo_S7(x)    8.05µs   9.13µs   101749.    10.8KB     30.5
#> 2 foo_S3(x)    2.58µs   2.81µs   322954.        0B     32.3
#> 3 foo_S4(x)    2.77µs   3.09µs   306280.        0B     30.6

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
#> 1 bar_S7(x, y)  14.59µs   16.2µs    59090.        0B     29.6
#> 2 bar_S4(x, y)   7.15µs   8.12µs   119781.        0B     12.0
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
#>  1 best                 3          15   8.14µs   9.38µs   103013.        0B     30.9
#>  2 worst                3          15   8.34µs   9.49µs   101597.        0B     30.5
#>  3 best                 5          15   8.07µs    9.2µs   104403.        0B     41.8
#>  4 worst                5          15    8.4µs   9.46µs   101715.        0B     30.5
#>  5 best                10          15   8.32µs   9.51µs   101514.        0B     30.5
#>  6 worst               10          15   8.66µs   9.85µs    97209.        0B     29.2
#>  7 best                50          15   8.39µs   9.61µs   100211.        0B     30.1
#>  8 worst               50          15  10.41µs  11.48µs    84088.        0B     25.2
#>  9 best               100          15   8.72µs  10.02µs    96336.        0B     38.5
#> 10 worst              100          15  12.61µs  13.95µs    68879.        0B     20.7
#> 11 best                 3         100   8.24µs   9.56µs   100292.        0B     30.1
#> 12 worst                3         100   8.63µs    9.8µs    99069.        0B     29.7
#> 13 best                 5         100   8.32µs   9.46µs   101924.        0B     30.6
#> 14 worst                5         100    8.7µs   9.87µs    97247.        0B     38.9
#> 15 best                10         100    8.2µs   9.34µs   103152.        0B     31.0
#> 16 worst               10         100   9.13µs   10.4µs    92309.        0B     27.7
#> 17 best                50         100   8.45µs     10µs    94574.        0B     28.4
#> 18 worst               50         100  13.89µs  15.28µs    63452.        0B     19.0
#> 19 best               100         100   8.78µs   9.95µs    96566.        0B     29.0
#> 20 worst              100         100  20.48µs  21.85µs    44215.        0B     13.3
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
#>  1 best                 3          15   10.5µs   12.4µs    77531.        0B     31.0
#>  2 worst                3          15   10.8µs   12.9µs    72728.        0B     29.1
#>  3 best                 5          15   10.4µs   12.1µs    79693.        0B     31.9
#>  4 worst                5          15   10.8µs   12.4µs    78165.        0B     31.3
#>  5 best                10          15   10.4µs   11.1µs    86761.        0B     34.7
#>  6 worst               10          15   11.1µs   11.9µs    79431.        0B     31.8
#>  7 best                50          15   10.6µs   11.4µs    85524.        0B     34.2
#>  8 worst               50          15   14.4µs   15.3µs    63328.        0B     25.3
#>  9 best               100          15   11.2µs   12.2µs    77612.        0B     31.1
#> 10 worst              100          15   18.9µs   20.3µs    47088.        0B     18.8
#> 11 best                 3         100   10.6µs   11.9µs    79150.        0B     31.7
#> 12 worst                3         100   11.4µs   13.1µs    71017.        0B     28.4
#> 13 best                 5         100   10.4µs     12µs    77167.        0B     30.9
#> 14 worst                5         100   11.5µs   12.7µs    75868.        0B     30.4
#> 15 best                10         100   10.4µs   11.5µs    83019.        0B     33.2
#> 16 worst               10         100   12.8µs   14.1µs    66310.        0B     26.5
#> 17 best                50         100   10.9µs   12.3µs    77412.        0B     31.0
#> 18 worst               50         100   21.3µs   22.9µs    41712.        0B     16.7
#> 19 best               100         100   11.5µs   13.1µs    71071.        0B     28.4
#> 20 worst              100         100   32.1µs   33.8µs    28336.        0B     11.3
```
