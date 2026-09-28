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
#> 1 foo_S7(x)    4.46µs   5.35µs   175952.    10.9KB     35.2
#> 2 foo_S3(x)    1.95µs   2.32µs   393245.        0B     39.3
#> 3 foo_S4(x)    2.09µs   2.54µs   375768.        0B     37.6

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
#> 1 bar_S7(x, y)    9.3µs  10.81µs    89299.        0B     26.8
#> 2 bar_S4(x, y)   5.47µs   6.58µs   146569.        0B     29.3
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
#>  1 best                 3          15   4.56µs   5.47µs   177004.        0B     35.4
#>  2 worst                3          15    4.6µs   5.57µs   168871.        0B     33.8
#>  3 best                 5          15   4.49µs   5.43µs   177075.        0B     35.4
#>  4 worst                5          15   4.69µs    5.6µs   171804.        0B     34.4
#>  5 best                10          15   4.53µs   5.45µs   175643.        0B     35.1
#>  6 worst               10          15   4.85µs   5.77µs   165124.        0B     16.5
#>  7 best                50          15   4.67µs   5.62µs   169923.        0B     34.0
#>  8 worst               50          15   6.01µs   6.91µs   138078.        0B     27.6
#>  9 best               100          15   4.93µs   6.23µs   109103.        0B     21.8
#> 10 worst              100          15   7.43µs   8.52µs   104823.        0B     21.0
#> 11 best                 3         100   4.62µs   5.56µs   173371.        0B     17.3
#> 12 worst                3         100   4.88µs   5.83µs   165405.        0B     33.1
#> 13 best                 5         100   4.59µs   5.59µs   171802.        0B     34.4
#> 14 worst                5         100   4.93µs   5.84µs   162788.        0B     32.6
#> 15 best                10         100    4.6µs   5.57µs   168877.        0B     33.8
#> 16 worst               10         100   5.16µs   6.14µs   156180.        0B     15.6
#> 17 best                50         100   4.71µs   5.65µs   168125.        0B     33.6
#> 18 worst               50         100   8.71µs   9.66µs   100069.        0B     20.0
#> 19 best               100         100   5.01µs   5.96µs   159846.        0B     32.0
#> 20 worst              100         100  13.26µs  14.27µs    68312.        0B     13.7
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
#>  1 best                 3          15   6.27µs   7.62µs   125240.        0B    37.6 
#>  2 worst                3          15   6.54µs   7.83µs   120899.        0B    36.3 
#>  3 best                 5          15   6.28µs   7.64µs   124764.        0B    25.0 
#>  4 worst                5          15   6.61µs   7.99µs   117635.        0B    35.3 
#>  5 best                10          15   6.29µs   7.73µs   121536.        0B    36.5 
#>  6 worst               10          15   6.92µs   8.31µs   114380.        0B    22.9 
#>  7 best                50          15   6.67µs   7.97µs   119417.        0B    23.9 
#>  8 worst               50          15   9.23µs  10.56µs    90396.        0B    27.1 
#>  9 best               100          15   7.04µs   8.46µs   113332.        0B    22.7 
#> 10 worst              100          15  12.37µs  13.72µs    70125.        0B    21.0 
#> 11 best                 3         100   6.44µs   7.83µs   120347.        0B    36.1 
#> 12 worst                3         100   7.12µs   8.49µs   112050.        0B    22.4 
#> 13 best                 5         100   6.35µs   7.74µs   119246.        0B    35.8 
#> 14 worst                5         100   7.12µs   8.51µs   109975.        0B    22.0 
#> 15 best                10         100   6.33µs   7.74µs   120994.        0B    24.2 
#> 16 worst               10         100   8.12µs   9.52µs    97946.        0B    29.4 
#> 17 best                50         100   6.75µs   8.18µs    74288.        0B    22.3 
#> 18 worst               50         100  14.06µs  14.76µs    66388.        0B    19.9 
#> 19 best               100         100   7.08µs    7.7µs   126438.        0B    37.9 
#> 20 worst              100         100  22.09µs  22.79µs    43200.        0B     8.64
```
