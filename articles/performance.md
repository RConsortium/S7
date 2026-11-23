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
#> 1 foo_S7(x)    6.64µs   7.55µs   124884.    10.9KB     25.0
#> 2 foo_S3(x)    2.57µs   2.96µs   307407.        0B     30.7
#> 3 foo_S4(x)    2.71µs   3.17µs   301355.        0B     30.1

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
#> 1 bar_S7(x, y)  14.11µs   15.8µs    60872.        0B     24.4
#> 2 bar_S4(x, y)   7.44µs   8.55µs   113829.        0B     22.8
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
#>  1 best                 3          15   6.46µs   7.59µs   127603.        0B    25.5 
#>  2 worst                3          15   6.84µs   7.81µs   123477.        0B    24.7 
#>  3 best                 5          15   6.61µs   7.61µs   124940.        0B    25.0 
#>  4 worst                5          15   6.82µs   7.92µs   122217.        0B    24.4 
#>  5 best                10          15   6.65µs   7.65µs   125453.        0B    25.1 
#>  6 worst               10          15   7.12µs   8.13µs   119153.        0B    23.8 
#>  7 best                50          15   6.84µs   7.96µs   116601.        0B    23.3 
#>  8 worst               50          15   9.05µs  10.17µs    95509.        0B    19.1 
#>  9 best               100          15   7.28µs   8.34µs   115771.        0B    23.2 
#> 10 worst              100          15  11.49µs  12.61µs    76920.        0B    15.4 
#> 11 best                 3         100   6.67µs   7.77µs   123315.        0B    24.7 
#> 12 worst                3         100   7.15µs   8.21µs   116151.        0B    23.2 
#> 13 best                 5         100    6.7µs   7.72µs   122985.        0B    24.6 
#> 14 worst                5         100   7.26µs   8.26µs   116207.        0B    23.2 
#> 15 best                10         100   6.64µs   7.75µs   119790.        0B    24.0 
#> 16 worst               10         100   7.67µs    8.7µs   110609.        0B    22.1 
#> 17 best                50         100   6.88µs   7.89µs   121035.        0B    24.2 
#> 18 worst               50         100  13.29µs  14.39µs    67094.        0B    13.4 
#> 19 best               100         100   7.28µs   8.37µs   114321.        0B    22.9 
#> 20 worst              100         100  20.87µs  22.02µs    44185.        0B     8.84
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
#>  1 best                 3          15   9.29µs   10.8µs    87181.        0B    26.2 
#>  2 worst                3          15   9.68µs   11.1µs    84872.        0B    25.5 
#>  3 best                 5          15   9.33µs   10.6µs    88506.        0B    26.6 
#>  4 worst                5          15   9.92µs   11.3µs    82865.        0B    24.9 
#>  5 best                10          15    9.3µs   10.8µs    87039.        0B    26.1 
#>  6 worst               10          15  10.33µs   11.8µs    81346.        0B    16.3 
#>  7 best                50          15   9.93µs   11.3µs    81663.        0B    24.5 
#>  8 worst               50          15  14.15µs   15.7µs    60887.        0B    18.3 
#>  9 best               100          15  10.32µs     12µs    77935.        0B    15.6 
#> 10 worst              100          15  19.28µs   20.8µs    46058.        0B    13.8 
#> 11 best                 3         100   9.58µs   11.2µs    84550.        0B    25.4 
#> 12 worst                3         100  10.57µs   12.2µs    74293.        0B    22.3 
#> 13 best                 5         100   9.42µs     11µs    85691.        0B    25.7 
#> 14 worst                5         100  10.31µs   11.5µs    83313.        0B    25.0 
#> 15 best                10         100   9.12µs   10.1µs    95252.        0B    28.6 
#> 16 worst               10         100   11.9µs   12.9µs    75375.        0B    22.6 
#> 17 best                50         100   9.99µs   10.9µs    89322.        0B    26.8 
#> 18 worst               50         100  21.77µs   22.6µs    42507.        0B    12.8 
#> 19 best               100         100   10.5µs   11.3µs    85644.        0B    25.7 
#> 20 worst              100         100   34.3µs   35.3µs    27701.        0B     8.31
```
