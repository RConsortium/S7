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
#> 1 foo_S7(x)    4.51µs   5.41µs   169106.    10.8KB     50.7
#> 2 foo_S3(x)    1.33µs   1.47µs   593210.        0B     59.3
#> 3 foo_S4(x)    1.46µs   1.63µs   561903.        0B     56.2

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
#> 1 bar_S7(x, y)   7.97µs   8.59µs   112707.        0B     56.4
#> 2 bar_S4(x, y)   3.87µs   4.25µs   227015.        0B     45.4
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
#>  1 best                 3          15   4.55µs   5.11µs   187596.        0B     56.3
#>  2 worst                3          15   4.67µs   5.14µs   188640.        0B     56.6
#>  3 best                 5          15    4.5µs   5.03µs   192264.        0B     57.7
#>  4 worst                5          15   4.69µs   5.16µs   187934.        0B     56.4
#>  5 best                10          15   4.58µs   5.01µs   193807.        0B     58.2
#>  6 worst               10          15    4.8µs   5.27µs   183827.        0B     55.2
#>  7 best                50          15   4.66µs   5.15µs   186579.        0B     56.0
#>  8 worst               50          15   5.77µs   6.25µs   154594.        0B     46.4
#>  9 best               100          15   4.83µs   5.33µs   181554.        0B     54.5
#> 10 worst              100          15   6.99µs    7.5µs   130309.        0B     52.1
#> 11 best                 3         100   4.59µs   5.11µs   189715.        0B     56.9
#> 12 worst                3         100   4.83µs   5.33µs   182347.        0B     73.0
#> 13 best                 5         100    4.6µs    5.1µs   189018.        0B     56.7
#> 14 worst                5         100   4.92µs   5.43µs   178196.        0B     53.5
#> 15 best                10         100   4.63µs   5.12µs   188488.        0B     56.6
#> 16 worst               10         100    5.1µs   5.63µs   171156.        0B     68.5
#> 17 best                50         100   4.59µs   5.12µs   187809.        0B     56.4
#> 18 worst               50         100   8.26µs   8.79µs   110980.        0B     44.4
#> 19 best               100         100   4.88µs   5.42µs   177023.        0B     53.1
#> 20 worst              100         100  12.49µs  13.03µs    75218.        0B     22.6
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
#>  1 best                 3          15    5.8µs   6.46µs   147477.        0B     59.0
#>  2 worst                3          15   6.06µs   6.65µs   144684.        0B     57.9
#>  3 best                 5          15   5.82µs   6.38µs   151118.        0B     75.6
#>  4 worst                5          15   6.14µs   6.58µs   146892.        0B     44.1
#>  5 best                10          15   5.83µs   6.14µs   159143.        0B     63.7
#>  6 worst               10          15   6.42µs   6.72µs   145101.        0B     58.1
#>  7 best                50          15   6.05µs   6.39µs   151963.        0B     60.8
#>  8 worst               50          15   8.25µs   8.62µs   113558.        0B     45.4
#>  9 best               100          15   6.28µs   6.68µs   143144.        0B     57.3
#> 10 worst              100          15  10.56µs  10.97µs    88721.        0B     35.5
#> 11 best                 3         100   5.92µs   6.28µs   154289.        0B     61.7
#> 12 worst                3         100   6.61µs   6.95µs   139424.        0B     55.8
#> 13 best                 5         100   5.77µs   6.21µs   155663.        0B     62.3
#> 14 worst                5         100   6.59µs   7.01µs   137873.        0B     55.2
#> 15 best                10         100   5.78µs   6.28µs   152381.        0B     61.0
#> 16 worst               10         100   7.48µs   7.82µs   123931.        0B     49.6
#> 17 best                50         100   6.23µs    6.7µs   143169.        0B     57.3
#> 18 worst               50         100  12.76µs   13.2µs    74024.        0B     29.6
#> 19 best               100         100   6.45µs   6.86µs   140068.        0B     56.0
#> 20 worst              100         100  19.92µs  20.48µs    47778.        0B     19.1
```
