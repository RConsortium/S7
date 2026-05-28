# Performance

``` r

library(S7)
```

The dispatch performance should be roughly on par with S3 and S4, though
as this is implemented in a package there is some overhead due to
`.Call` vs `.Primitive`.

``` r

Text <- new_class("Text", parent = class_character)
Number <- new_class("Number", parent = class_double)

x <- Text("hi")
y <- Number(1)

foo_S7 <- new_generic("foo_S7", "x")
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
#> 1 foo_S7(x)    7.18µs   8.66µs   108013.    10.8KB     21.6
#> 2 foo_S3(x)     2.5µs   2.85µs   318999.        0B      0  
#> 3 foo_S4(x)    2.68µs   3.19µs   300194.        0B     30.0

bar_S7 <- new_generic("bar_S7", c("x", "y"))
method(bar_S7, list(Text, Number)) <- function(x, y, ...) paste0(x, "-", y, "-bar")

setGeneric("bar_S4", function(x, y, ...) standardGeneric("bar_S4"))
#> [1] "bar_S4"
setMethod("bar_S4", c("Text", "Number"), function(x, y, ...) paste0(x, "-", y, "-bar"))

# Measure performance of double dispatch
bench::mark(bar_S7(x, y), bar_S4(x, y))
#> # A tibble: 2 × 6
#>   expression        min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>   <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 bar_S7(x, y)  12.95µs  14.89µs    65174.        0B     19.6
#> 2 bar_S4(x, y)   6.93µs   7.96µs   122038.        0B     24.4
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
    Text <- new_class("Text", parent = class_character)
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
    foo_S7 <- new_generic("foo_S7", "x")
    method(foo_S7, cls) <- function(x, ...) paste0(x, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 <- new_generic("foo2_S7", "x")
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
#>  1 best                 3          15   7.44µs   9.05µs   106567.        0B     21.3
#>  2 worst                3          15   7.51µs   9.16µs   105683.        0B     31.7
#>  3 best                 5          15   7.44µs   8.98µs   107610.        0B     32.3
#>  4 worst                5          15   7.69µs   9.13µs   105669.        0B     31.7
#>  5 best                10          15   7.45µs   9.03µs   106940.        0B     32.1
#>  6 worst               10          15   7.89µs   9.37µs   103553.        0B     20.7
#>  7 best                50          15   8.01µs   9.78µs    99121.        0B     29.7
#>  8 worst               50          15   9.72µs  11.47µs    84256.        0B     25.3
#>  9 best               100          15   8.67µs  10.27µs    93354.        0B     28.0
#> 10 worst              100          15  11.92µs  13.55µs    71255.        0B     21.4
#> 11 best                 3         100   7.54µs   9.05µs   106863.        0B     32.1
#> 12 worst                3         100   7.77µs   9.44µs   102696.        0B     30.8
#> 13 best                 5         100   7.49µs   9.16µs   105479.        0B     31.7
#> 14 worst                5         100   7.79µs   9.59µs    89724.        0B     26.9
#> 15 best                10         100   7.49µs   9.72µs    79800.        0B     23.9
#> 16 worst               10         100   8.24µs  10.04µs    95308.        0B     28.6
#> 17 best                50         100   8.09µs   9.84µs    96913.        0B     29.1
#> 18 worst               50         100   13.2µs  14.94µs    64536.        0B     19.4
#> 19 best               100         100   9.09µs  10.76µs    88707.        0B     26.6
#> 20 worst              100         100  19.51µs  21.36µs    45551.        0B     13.7
```

And the same benchmark using double-dispatch

``` r

bench::press(
  num_classes = c(3, 5, 10, 50, 100),
  class_nchar = c(15, 100),
  {
    # Construct a class hierarchy with that number of classes
    Text <- new_class("Text", parent = class_character)
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
    foo_S7 <- new_generic("foo_S7", c("x", "y"))
    method(foo_S7, list(cls, cls)) <- function(x, y, ...) paste0(x, y, "-foo")

    # Define a generic and a method for the first class (worst case scenario)
    foo2_S7 <- new_generic("foo2_S7", c("x", "y"))
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
#>  1 best                 3          15    9.2µs     11µs    86220.        0B    34.5 
#>  2 worst                3          15   9.52µs   11.2µs    85927.        0B    25.8 
#>  3 best                 5          15   9.41µs   11.2µs    85631.        0B    25.7 
#>  4 worst                5          15   9.71µs   11.6µs    82355.        0B    33.0 
#>  5 best                10          15   9.36µs   11.1µs    86277.        0B    25.9 
#>  6 worst               10          15   9.83µs   10.8µs    88269.        0B    35.3 
#>  7 best                50          15  10.37µs   11.2µs    87277.        0B    34.9 
#>  8 worst               50          15  13.57µs   14.3µs    68587.        0B    20.6 
#>  9 best               100          15  11.57µs   12.3µs    79358.        0B    31.8 
#> 10 worst              100          15  18.31µs   19.5µs    49997.        0B    20.0 
#> 11 best                 3         100   9.48µs   10.8µs    89881.        0B    27.0 
#> 12 worst                3         100  10.28µs   11.6µs    83205.        0B    33.3 
#> 13 best                 5         100   9.34µs   10.6µs    91815.        0B    27.6 
#> 14 worst                5         100  10.35µs   11.6µs    83711.        0B    33.5 
#> 15 best                10         100   9.53µs   10.8µs    86727.        0B    26.0 
#> 16 worst               10         100  11.81µs   13.1µs    73904.        0B    29.6 
#> 17 best                50         100  10.71µs     12µs    80109.        0B    32.1 
#> 18 worst               50         100  20.12µs   21.5µs    45504.        0B    13.7 
#> 19 best               100         100  11.68µs   12.9µs    75088.        0B    37.6 
#> 20 worst              100         100  31.26µs     33µs    29466.        0B     8.84
```
