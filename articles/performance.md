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
#> 1 foo_S7(x)    7.44µs   9.36µs   100677.    10.8KB     30.2
#> 2 foo_S3(x)    2.49µs   2.92µs   311390.        0B     31.1
#> 3 foo_S4(x)    2.71µs   3.27µs   293652.        0B     29.4

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
#> 1 bar_S7(x, y)  13.05µs   15.2µs    63543.        0B     25.4
#> 2 bar_S4(x, y)   6.92µs    8.1µs   120194.        0B     24.0
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
#>  1 best                 3          15    7.6µs   9.23µs   104565.        0B    31.4 
#>  2 worst                3          15   7.72µs   9.55µs    97894.        0B    29.4 
#>  3 best                 5          15   7.52µs   9.36µs   103307.        0B    31.0 
#>  4 worst                5          15    7.7µs   9.56µs   101451.        0B    30.4 
#>  5 best                10          15   7.68µs   9.43µs   102400.        0B    30.7 
#>  6 worst               10          15      8µs    9.7µs    99836.        0B    30.0 
#>  7 best                50          15   8.19µs   9.96µs    97394.        0B    29.2 
#>  8 worst               50          15   9.76µs  11.53µs    84335.        0B    25.3 
#>  9 best               100          15   8.86µs  10.61µs    91431.        0B    27.4 
#> 10 worst              100          15  12.06µs  13.73µs    70884.        0B    21.3 
#> 11 best                 3         100   7.67µs   9.22µs   104817.        0B    31.5 
#> 12 worst                3         100      8µs   9.63µs   100230.        0B    30.1 
#> 13 best                 5         100   7.83µs    9.4µs   102226.        0B    30.7 
#> 14 worst                5         100    8.1µs   9.87µs    95903.        0B    28.8 
#> 15 best                10         100   7.67µs   9.46µs   100786.        0B    20.2 
#> 16 worst               10         100   8.43µs   10.2µs    93514.        0B    28.1 
#> 17 best                50         100   8.03µs   9.85µs    96861.        0B    29.1 
#> 18 worst               50         100  13.26µs  15.02µs    64043.        0B    19.2 
#> 19 best               100         100   8.84µs  10.45µs    91752.        0B    27.5 
#> 20 worst              100         100  19.61µs  21.42µs    45403.        0B     9.08
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
#>  1 best                 3          15   9.43µs   11.6µs    82110.        0B     32.9
#>  2 worst                3          15   9.62µs   11.6µs    82718.        0B     33.1
#>  3 best                 5          15   9.22µs   11.5µs    82716.        0B     33.1
#>  4 worst                5          15   9.53µs   11.4µs    83088.        0B     33.2
#>  5 best                10          15   9.31µs   10.2µs    95465.        0B     38.2
#>  6 worst               10          15  10.05µs   10.8µs    90104.        0B     27.0
#>  7 best                50          15  10.49µs   11.3µs    86337.        0B     34.5
#>  8 worst               50          15   13.8µs   14.7µs    66139.        0B     19.8
#>  9 best               100          15  11.92µs   13.5µs    70272.        0B     28.1
#> 10 worst              100          15  18.38µs   20.3µs    46913.        0B     18.8
#> 11 best                 3         100   9.53µs   11.1µs    86020.        0B     34.4
#> 12 worst                3         100  10.53µs   12.1µs    78950.        0B     31.6
#> 13 best                 5         100   9.38µs   11.1µs    85869.        0B     34.4
#> 14 worst                5         100  10.41µs   12.3µs    77060.        0B     30.8
#> 15 best                10         100   9.55µs   11.3µs    83503.        0B     33.4
#> 16 worst               10         100  11.74µs   13.6µs    70352.        0B     21.1
#> 17 best                50         100  10.89µs   12.5µs    76165.        0B     30.5
#> 18 worst               50         100  20.12µs   21.8µs    44424.        0B     17.8
#> 19 best               100         100  11.85µs   13.7µs    69184.        0B     34.6
#> 20 worst              100         100  31.33µs   33.2µs    29383.        0B     11.8
```
