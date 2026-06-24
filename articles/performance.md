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
#> 1 foo_S7(x)    7.26µs   8.81µs   107309.    10.8KB     32.2
#> 2 foo_S3(x)    2.52µs   2.87µs   319336.        0B     31.9
#> 3 foo_S4(x)    2.73µs   3.19µs   301240.        0B     30.1

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
#> 1 bar_S7(x, y)  13.14µs   15.1µs    64480.        0B     25.8
#> 2 bar_S4(x, y)   6.93µs   8.04µs   121121.        0B     24.2
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
#>  1 best                 3          15    7.2µs   8.81µs   109732.        0B     32.9
#>  2 worst                3          15   7.58µs   9.14µs   105868.        0B     31.8
#>  3 best                 5          15   7.42µs      9µs   107955.        0B     32.4
#>  4 worst                5          15   7.78µs   9.31µs   104148.        0B     31.3
#>  5 best                10          15   7.56µs   9.15µs   106285.        0B     31.9
#>  6 worst               10          15   8.02µs   9.51µs   102030.        0B     30.6
#>  7 best                50          15   8.07µs   9.64µs   100530.        0B     30.2
#>  8 worst               50          15   9.83µs  11.29µs    86014.        0B     25.8
#>  9 best               100          15   8.69µs  10.29µs    94110.        0B     28.2
#> 10 worst              100          15  11.86µs  13.51µs    71979.        0B     21.6
#> 11 best                 3         100   7.59µs   9.07µs   106766.        0B     32.0
#> 12 worst                3         100   7.91µs   9.37µs   102957.        0B     30.9
#> 13 best                 5         100   7.65µs   9.18µs   104483.        0B     31.4
#> 14 worst                5         100   8.07µs   9.62µs    99556.        0B     29.9
#> 15 best                10         100   7.78µs   9.31µs   102547.        0B     30.8
#> 16 worst               10         100   8.51µs   10.1µs    94681.        0B     28.4
#> 17 best                50         100   8.04µs   9.58µs    99757.        0B     29.9
#> 18 worst               50         100  13.19µs  14.85µs    64993.        0B     19.5
#> 19 best               100         100   8.74µs  10.41µs    91460.        0B     18.3
#> 20 worst              100         100   19.4µs  21.11µs    46041.        0B     13.8
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
#>  1 best                 3          15      9µs     11µs    86489.        0B     34.6
#>  2 worst                3          15   9.58µs   11.5µs    82561.        0B     33.0
#>  3 best                 5          15   9.23µs   11.1µs    86013.        0B     34.4
#>  4 worst                5          15   9.36µs   11.2µs    86272.        0B     34.5
#>  5 best                10          15   9.54µs   10.3µs    95029.        0B     38.0
#>  6 worst               10          15  10.29µs     11µs    88994.        0B     26.7
#>  7 best                50          15  10.43µs   11.2µs    87221.        0B     34.9
#>  8 worst               50          15  13.45µs   14.3µs    68582.        0B     20.6
#>  9 best               100          15  11.66µs   12.9µs    74327.        0B     29.7
#> 10 worst              100          15  18.03µs   19.6µs    49646.        0B     19.9
#> 11 best                 3         100   9.38µs   10.6µs    89552.        0B     35.8
#> 12 worst                3         100   10.2µs   11.4µs    83594.        0B     33.5
#> 13 best                 5         100   9.38µs   10.7µs    89206.        0B     35.7
#> 14 worst                5         100  10.46µs   11.8µs    79982.        0B     32.0
#> 15 best                10         100   9.59µs     11µs    86261.        0B     34.5
#> 16 worst               10         100  11.82µs   13.3µs    71490.        0B     28.6
#> 17 best                50         100  10.92µs   12.3µs    76861.        0B     30.8
#> 18 worst               50         100  20.25µs   21.8µs    44352.        0B     17.7
#> 19 best               100         100  11.87µs   13.5µs    70912.        0B     28.4
#> 20 worst              100         100  31.16µs   33.1µs    29365.        0B     14.7
```
