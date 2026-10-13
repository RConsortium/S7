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
#> 1 foo_S7(x)    5.96µs   6.87µs   135622.    10.9KB     27.1
#> 2 foo_S3(x)    2.49µs   2.84µs   319783.        0B     32.0
#> 3 foo_S4(x)    2.67µs    3.1µs   310181.        0B     31.0

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
#> 1 bar_S7(x, y)  12.27µs  13.78µs    69909.        0B     28.0
#> 2 bar_S4(x, y)   6.88µs   7.88µs   123261.        0B     24.7
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
#>  1 best                 3          15   6.08µs   7.01µs   137502.        0B     27.5
#>  2 worst                3          15   6.27µs   7.24µs   133758.        0B     26.8
#>  3 best                 5          15   6.07µs   7.08µs   135928.        0B     27.2
#>  4 worst                5          15   6.33µs   7.21µs   133969.        0B     26.8
#>  5 best                10          15   6.07µs   6.87µs   140639.        0B     28.1
#>  6 worst               10          15    6.5µs   7.27µs   133547.        0B     26.7
#>  7 best                50          15   6.21µs   7.26µs   132755.        0B     26.6
#>  8 worst               50          15   7.98µs   8.83µs   110329.        0B     22.1
#>  9 best               100          15    6.6µs   7.48µs   129633.        0B     25.9
#> 10 worst              100          15   9.85µs  10.77µs    90284.        0B     18.1
#> 11 best                 3         100    6.2µs   7.24µs   133915.        0B     13.4
#> 12 worst                3         100   6.55µs   7.53µs   128035.        0B     25.6
#> 13 best                 5         100   6.24µs   7.21µs   133024.        0B     13.3
#> 14 worst                5         100   6.68µs   7.53µs   126223.        0B     25.2
#> 15 best                10         100   6.17µs   7.07µs   136335.        0B     13.6
#> 16 worst               10         100   6.93µs   7.85µs   123261.        0B     24.7
#> 17 best                50         100   6.26µs    7.2µs   133091.        0B     26.6
#> 18 worst               50         100  11.46µs  12.37µs    78670.        0B     15.7
#> 19 best               100         100   6.75µs   7.69µs   125616.        0B     25.1
#> 20 worst              100         100  17.48µs  18.44µs    52964.        0B     10.6
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
#>  1 best                 3          15    8.6µs   9.85µs    97613.        0B    29.3 
#>  2 worst                3          15   8.89µs  10.04µs    96301.        0B    28.9 
#>  3 best                 5          15   8.57µs   9.88µs    97296.        0B    29.2 
#>  4 worst                5          15   9.03µs  10.18µs    95040.        0B    28.5 
#>  5 best                10          15   8.71µs   9.91µs    96890.        0B    29.1 
#>  6 worst               10          15   9.47µs  10.79µs    88442.        0B    26.5 
#>  7 best                50          15   8.96µs  10.49µs    91258.        0B    27.4 
#>  8 worst               50          15  12.33µs  13.86µs    69386.        0B    20.8 
#>  9 best               100          15   9.64µs  11.08µs    86828.        0B    26.1 
#> 10 worst              100          15  16.46µs   17.8µs    54781.        0B    11.0 
#> 11 best                 3         100   8.76µs  10.12µs    95161.        0B    19.0 
#> 12 worst                3         100   9.63µs  11.09µs    86300.        0B    25.9 
#> 13 best                 5         100   8.54µs   9.72µs    99380.        0B    19.9 
#> 14 worst                5         100    9.6µs  10.24µs    95144.        0B    28.6 
#> 15 best                10         100   8.58µs   9.26µs   104925.        0B    31.5 
#> 16 worst               10         100  10.85µs  11.52µs    84802.        0B    25.4 
#> 17 best                50         100   9.22µs   9.91µs    98200.        0B    19.6 
#> 18 worst               50         100  18.64µs  19.33µs    50645.        0B    15.2 
#> 19 best               100         100   9.63µs  10.27µs    95027.        0B    28.5 
#> 20 worst              100         100  28.88µs  29.72µs    32964.        0B     9.89
```
