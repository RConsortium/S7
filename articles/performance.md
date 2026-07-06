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
#> 1 foo_S7(x)    7.47µs   8.56µs   109640.    10.8KB     32.9
#> 2 foo_S3(x)    2.54µs   2.81µs   324737.        0B     32.5
#> 3 foo_S4(x)    2.71µs   3.11µs   305169.        0B     30.5

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
#> 1 bar_S7(x, y)  13.59µs  15.07µs    63920.        0B     25.6
#> 2 bar_S4(x, y)   7.11µs   7.97µs   121633.        0B     24.3
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
#>  1 best                 3          15    7.5µs   8.77µs   110685.        0B     33.2
#>  2 worst                3          15   7.75µs   9.06µs   106145.        0B     31.9
#>  3 best                 5          15   7.67µs   8.99µs   107340.        0B     32.2
#>  4 worst                5          15   8.01µs   9.21µs   105495.        0B     31.7
#>  5 best                10          15   7.83µs   9.05µs   107238.        0B     32.2
#>  6 worst               10          15   8.19µs   9.34µs   103933.        0B     31.2
#>  7 best                50          15   8.18µs   9.46µs   102534.        0B     30.8
#>  8 worst               50          15  10.35µs  11.69µs    82720.        0B     24.8
#>  9 best               100          15   8.68µs  10.18µs    94069.        0B     28.2
#> 10 worst              100          15   12.7µs  14.24µs    68132.        0B     20.4
#> 11 best                 3         100   7.62µs   8.95µs   108144.        0B     32.5
#> 12 worst                3         100   8.09µs   9.31µs   103876.        0B     41.6
#> 13 best                 5         100   7.74µs      9µs   106137.        0B     31.9
#> 14 worst                5         100   8.22µs   9.43µs   100433.        0B     30.1
#> 15 best                10         100   7.75µs   9.03µs   105665.        0B     31.7
#> 16 worst               10         100    8.6µs   9.92µs    96140.        0B     28.9
#> 17 best                50         100   8.06µs   9.48µs   101195.        0B     30.4
#> 18 worst               50         100   13.7µs  15.11µs    63856.        0B     19.2
#> 19 best               100         100   8.72µs  10.19µs    93856.        0B     28.2
#> 20 worst              100         100  20.41µs     22µs    43869.        0B     13.2
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
#>  1 best                 3          15   9.49µs     11µs    86811.        0B     34.7
#>  2 worst                3          15    9.9µs   11.2µs    86580.        0B     34.6
#>  3 best                 5          15   9.47µs   10.9µs    88496.        0B     35.4
#>  4 worst                5          15   9.84µs   11.2µs    86561.        0B     26.0
#>  5 best                10          15   9.48µs   10.1µs    96260.        0B     28.9
#>  6 worst               10          15  10.26µs   10.9µs    88622.        0B     35.5
#>  7 best                50          15  10.39µs     11µs    88365.        0B     26.5
#>  8 worst               50          15     14µs   14.8µs    65920.        0B     26.4
#>  9 best               100          15  11.67µs   12.7µs    76156.        0B     30.5
#> 10 worst              100          15  19.43µs   20.4µs    47492.        0B     19.0
#> 11 best                 3         100   9.68µs   10.7µs    90290.        0B     36.1
#> 12 worst                3         100  10.49µs   11.4µs    84362.        0B     42.2
#> 13 best                 5         100    9.5µs   10.4µs    92571.        0B     37.0
#> 14 worst                5         100  10.46µs   11.6µs    83379.        0B     25.0
#> 15 best                10         100   9.62µs   10.7µs    89699.        0B     26.9
#> 16 worst               10         100  11.85µs     13µs    73676.        0B     29.5
#> 17 best                50         100  10.78µs   11.9µs    81061.        0B     32.4
#> 18 worst               50         100  20.75µs     22µs    43934.        0B     13.2
#> 19 best               100         100   11.9µs   13.1µs    73447.        0B     36.7
#> 20 worst              100         100  32.52µs   33.8µs    28813.        0B     14.4
```
