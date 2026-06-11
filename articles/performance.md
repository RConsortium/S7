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
#> 1 foo_S7(x)    7.34µs   8.93µs   105565.    10.8KB     31.7
#> 2 foo_S3(x)    2.46µs   2.85µs   317598.        0B     31.8
#> 3 foo_S4(x)     2.7µs   3.23µs   296856.        0B     29.7

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
#> 1 bar_S7(x, y)  12.85µs  15.12µs    63465.        0B     25.4
#> 2 bar_S4(x, y)   7.02µs   8.12µs   119873.        0B     24.0
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
#>  1 best                 3          15   7.56µs   9.16µs   105896.        0B     31.8
#>  2 worst                3          15    7.7µs   9.23µs   105178.        0B     31.6
#>  3 best                 5          15   7.41µs   8.99µs   107809.        0B     32.4
#>  4 worst                5          15   7.62µs   9.11µs   106278.        0B     31.9
#>  5 best                10          15   7.56µs   9.14µs   105776.        0B     31.7
#>  6 worst               10          15   7.88µs   9.43µs   102854.        0B     30.9
#>  7 best                50          15   8.15µs   9.79µs    98861.        0B     29.7
#>  8 worst               50          15   9.72µs  11.31µs    85814.        0B     25.8
#>  9 best               100          15    8.8µs  10.44µs    92653.        0B     37.1
#> 10 worst              100          15  12.11µs  13.75µs    70897.        0B     21.3
#> 11 best                 3         100    7.7µs   9.29µs   103834.        0B     41.6
#> 12 worst                3         100   7.94µs   9.55µs   101347.        0B     30.4
#> 13 best                 5         100   7.69µs   9.31µs   103243.        0B     31.0
#> 14 worst                5         100   8.06µs   9.66µs    98944.        0B     29.7
#> 15 best                10         100   7.71µs   9.31µs   102437.        0B     30.7
#> 16 worst               10         100   8.41µs  10.01µs    95693.        0B     28.7
#> 17 best                50         100    8.1µs   9.77µs    97872.        0B     29.4
#> 18 worst               50         100  13.25µs  14.93µs    64697.        0B     19.4
#> 19 best               100         100    8.9µs  10.76µs    88920.        0B     26.7
#> 20 worst              100         100  19.56µs  21.51µs    45035.        0B     13.5
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
#>  1 best                 3          15   9.57µs   11.7µs    81746.        0B     24.5
#>  2 worst                3          15   9.68µs   11.9µs    79540.        0B     31.8
#>  3 best                 5          15   9.39µs   11.9µs    64474.        0B     25.8
#>  4 worst                5          15   9.49µs   11.7µs    80785.        0B     32.3
#>  5 best                10          15   9.42µs   10.2µs    95326.        0B     38.1
#>  6 worst               10          15  10.07µs   10.9µs    89804.        0B     35.9
#>  7 best                50          15  10.59µs   11.4µs    85456.        0B     34.2
#>  8 worst               50          15  13.74µs   14.6µs    66846.        0B     26.7
#>  9 best               100          15  11.79µs   13.1µs    73100.        0B     29.3
#> 10 worst              100          15  18.18µs   19.6µs    49431.        0B     19.8
#> 11 best                 3         100   9.41µs   10.8µs    88563.        0B     35.4
#> 12 worst                3         100  10.35µs   11.7µs    82292.        0B     32.9
#> 13 best                 5         100   9.24µs   10.6µs    89967.        0B     36.0
#> 14 worst                5         100  10.12µs   11.6µs    80912.        0B     32.4
#> 15 best                10         100   9.57µs   10.9µs    87353.        0B     35.0
#> 16 worst               10         100  11.74µs   13.1µs    72757.        0B     29.1
#> 17 best                50         100  10.67µs   12.1µs    79176.        0B     31.7
#> 18 worst               50         100  19.87µs   21.4µs    45279.        0B     22.7
#> 19 best               100         100  11.91µs   13.2µs    72413.        0B     43.5
#> 20 worst              100         100  31.06µs   32.5µs    29953.        0B     15.0
```
