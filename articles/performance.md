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
#> 1 foo_S7(x)    7.25µs   8.54µs   109655.    10.8KB     32.9
#> 2 foo_S3(x)    2.51µs   2.81µs   325123.        0B     32.5
#> 3 foo_S4(x)    2.77µs   3.19µs   301478.        0B     30.2

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
#> 1 bar_S7(x, y)  12.84µs  14.58µs    66735.        0B     26.7
#> 2 bar_S4(x, y)   7.03µs   7.92µs   123074.        0B     24.6
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
#>  1 best                 3          15   7.53µs   8.99µs   108005.        0B     32.4
#>  2 worst                3          15   7.67µs   9.09µs   106565.        0B     32.0
#>  3 best                 5          15   7.64µs   8.83µs   109917.        0B     33.0
#>  4 worst                5          15   7.67µs   8.83µs   109724.        0B     32.9
#>  5 best                10          15   7.62µs   8.78µs   110632.        0B     33.2
#>  6 worst               10          15   7.99µs   9.07µs   107001.        0B     32.1
#>  7 best                50          15   8.04µs   9.26µs   104629.        0B     31.4
#>  8 worst               50          15    9.6µs  10.85µs    89611.        0B     26.9
#>  9 best               100          15   8.69µs  10.31µs    93999.        0B     28.2
#> 10 worst              100          15     12µs  13.41µs    72382.        0B     21.7
#> 11 best                 3         100   7.62µs   8.68µs   111382.        0B     44.6
#> 12 worst                3         100   7.95µs      9µs   107856.        0B     32.4
#> 13 best                 5         100   7.63µs   9.04µs   106028.        0B     31.8
#> 14 worst                5         100   7.95µs   9.16µs   104679.        0B     31.4
#> 15 best                10         100    7.7µs   8.86µs   108188.        0B     32.5
#> 16 worst               10         100   8.38µs   9.57µs   100521.        0B     30.2
#> 17 best                50         100   8.08µs   9.35µs   102926.        0B     30.9
#> 18 worst               50         100  13.14µs   14.5µs    66701.        0B     20.0
#> 19 best               100         100    8.7µs  10.04µs    95757.        0B     28.7
#> 20 worst              100         100  19.41µs  21.02µs    46000.        0B     13.8
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
#>  1 best                 3          15   9.39µs     11µs    87084.        0B     34.8
#>  2 worst                3          15   9.69µs   11.2µs    85733.        0B     34.3
#>  3 best                 5          15   9.25µs   10.8µs    88899.        0B     35.6
#>  4 worst                5          15   9.52µs   10.8µs    89134.        0B     35.7
#>  5 best                10          15   9.56µs   10.2µs    95977.        0B     28.8
#>  6 worst               10          15  10.12µs   10.7µs    91356.        0B     36.6
#>  7 best                50          15   10.3µs   10.9µs    89653.        0B     26.9
#>  8 worst               50          15  13.49µs   14.1µs    69207.        0B     27.7
#>  9 best               100          15  11.77µs   12.7µs    75878.        0B     30.4
#> 10 worst              100          15  18.33µs   19.3µs    50365.        0B     20.2
#> 11 best                 3         100   9.46µs   10.4µs    92941.        0B     46.5
#> 12 worst                3         100   10.4µs   11.4µs    84785.        0B     33.9
#> 13 best                 5         100   9.38µs   10.4µs    91330.        0B     36.5
#> 14 worst                5         100  10.45µs   11.5µs    82767.        0B     33.1
#> 15 best                10         100   9.56µs   10.6µs    89738.        0B     35.9
#> 16 worst               10         100   11.7µs   12.9µs    74579.        0B     22.4
#> 17 best                50         100  10.95µs   12.2µs    78465.        0B     31.4
#> 18 worst               50         100  20.25µs   21.8µs    44473.        0B     17.8
#> 19 best               100         100  11.97µs   13.1µs    73452.        0B     36.7
#> 20 worst              100         100  31.49µs     33µs    29416.        0B     11.8
```
