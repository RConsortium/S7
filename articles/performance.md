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
#> 1 foo_S7(x)    4.45µs   5.36µs   176253.    10.9KB     35.3
#> 2 foo_S3(x)    1.93µs   2.27µs   401433.        0B     40.1
#> 3 foo_S4(x)    2.08µs   2.51µs   381253.        0B     38.1

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
#> 1 bar_S7(x, y)   9.27µs   10.6µs    91071.        0B     27.3
#> 2 bar_S4(x, y)   5.42µs    6.4µs   151977.        0B     30.4
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
#>  1 best                 3          15   4.52µs    5.4µs   175252.        0B    35.1 
#>  2 worst                3          15   4.66µs    5.5µs   174814.        0B    35.0 
#>  3 best                 5          15   4.54µs   5.47µs   174963.        0B    35.0 
#>  4 worst                5          15   4.76µs   5.68µs   169207.        0B    33.8 
#>  5 best                10          15   4.59µs   5.51µs   174551.        0B    34.9 
#>  6 worst               10          15   4.88µs   5.71µs   169104.        0B    16.9 
#>  7 best                50          15   4.73µs    5.6µs   172281.        0B    34.5 
#>  8 worst               50          15   6.06µs   6.94µs   140146.        0B    14.0 
#>  9 best               100          15   4.89µs    5.7µs   169670.        0B    33.9 
#> 10 worst              100          15   7.42µs   8.23µs   118162.        0B    11.8 
#> 11 best                 3         100   4.63µs   5.49µs   175784.        0B    35.2 
#> 12 worst                3         100   4.84µs   5.66µs   170979.        0B    34.2 
#> 13 best                 5         100   4.63µs   5.56µs   173955.        0B    34.8 
#> 14 worst                5         100      5µs   5.81µs   165506.        0B    33.1 
#> 15 best                10         100   4.61µs   5.43µs   176246.        0B    35.3 
#> 16 worst               10         100   5.23µs   5.99µs   161787.        0B    16.2 
#> 17 best                50         100   4.75µs   5.48µs   175291.        0B    35.1 
#> 18 worst               50         100   8.73µs   9.56µs   101905.        0B    10.2 
#> 19 best               100         100   5.01µs   5.81µs   165872.        0B    33.2 
#> 20 worst              100         100  13.34µs  14.31µs    68565.        0B     6.86
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
#>  1 best                 3          15   6.26µs   7.42µs   129544.        0B     38.9
#>  2 worst                3          15   6.48µs   7.63µs   127145.        0B     25.4
#>  3 best                 5          15   6.34µs   7.65µs   123708.        0B     37.1
#>  4 worst                5          15   6.63µs    7.8µs   123663.        0B     24.7
#>  5 best                10          15   6.29µs   7.56µs   124814.        0B     25.0
#>  6 worst               10          15   6.93µs   8.15µs   117531.        0B     35.3
#>  7 best                50          15   6.69µs   7.88µs   122236.        0B     24.5
#>  8 worst               50          15   9.25µs  10.57µs    91084.        0B     27.3
#>  9 best               100          15   7.11µs   8.39µs   114011.        0B     34.2
#> 10 worst              100          15  12.23µs  13.55µs    71996.        0B     14.4
#> 11 best                 3         100   6.54µs   7.72µs   124344.        0B     24.9
#> 12 worst                3         100   7.18µs   8.28µs   115914.        0B     34.8
#> 13 best                 5         100   6.36µs    7.5µs   128500.        0B     25.7
#> 14 worst                5         100   7.06µs   8.38µs   114240.        0B     34.3
#> 15 best                10         100   6.38µs   7.57µs   126123.        0B     37.8
#> 16 worst               10         100   8.17µs   9.36µs   103355.        0B     20.7
#> 17 best                50         100   6.76µs   7.67µs   125250.        0B     37.6
#> 18 worst               50         100  14.19µs  14.73µs    66789.        0B     13.4
#> 19 best               100         100    7.1µs   7.69µs   126623.        0B     25.3
#> 20 worst              100         100  22.09µs  22.73µs    43189.        0B     13.0
```
