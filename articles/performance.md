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
#> 1 foo_S7(x)     4.8µs   5.75µs   163214.    10.9KB     32.6
#> 2 foo_S3(x)    1.96µs   2.41µs   375738.        0B     37.6
#> 3 foo_S4(x)    2.06µs    2.6µs   366241.        0B     36.6

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
#> 1 bar_S7(x, y)  10.04µs   11.7µs    82297.        0B     32.9
#> 2 bar_S4(x, y)   5.52µs    6.7µs   144413.        0B     28.9
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
#>  1 best                 3          15   4.76µs   5.89µs   153139.        0B     30.6
#>  2 worst                3          15   4.87µs   5.93µs   161532.        0B     32.3
#>  3 best                 5          15   4.77µs   5.86µs   163269.        0B     32.7
#>  4 worst                5          15   4.95µs   6.05µs   154768.        0B     31.0
#>  5 best                10          15   4.77µs   5.89µs   162093.        0B     32.4
#>  6 worst               10          15   5.13µs    6.2µs   154913.        0B     31.0
#>  7 best                50          15   4.96µs   6.06µs   158401.        0B     31.7
#>  8 worst               50          15    6.5µs   7.64µs   126243.        0B     25.3
#>  9 best               100          15   5.22µs   6.31µs   152091.        0B     30.4
#> 10 worst              100          15   8.23µs   9.34µs   103885.        0B     20.8
#> 11 best                 3         100   4.92µs   6.02µs   158179.        0B     31.6
#> 12 worst                3         100   5.22µs   6.33µs   149381.        0B     29.9
#> 13 best                 5         100   4.94µs   6.06µs   156002.        0B     31.2
#> 14 worst                5         100   5.29µs   6.42µs   147469.        0B     29.5
#> 15 best                10         100   4.84µs   5.99µs   158051.        0B     31.6
#> 16 worst               10         100    5.6µs   6.73µs   141218.        0B     28.2
#> 17 best                50         100   5.03µs    6.1µs   154914.        0B     31.0
#> 18 worst               50         100  10.04µs  11.16µs    86405.        0B     17.3
#> 19 best               100         100   5.37µs   6.45µs   146848.        0B     29.4
#> 20 worst              100         100  15.86µs  17.02µs    57267.        0B     11.5
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
#>  1 best                 3          15   6.89µs   8.42µs   109983.        0B     33.0
#>  2 worst                3          15   7.08µs   8.67µs   107738.        0B     32.3
#>  3 best                 5          15   6.78µs   8.41µs   110589.        0B     33.2
#>  4 worst                5          15   7.18µs   8.79µs   106416.        0B     31.9
#>  5 best                10          15   6.87µs   8.48µs   109627.        0B     32.9
#>  6 worst               10          15   7.47µs   9.19µs   101532.        0B     30.5
#>  7 best                50          15   7.29µs   8.82µs   106172.        0B     31.9
#>  8 worst               50          15  10.19µs  11.72µs    80904.        0B     24.3
#>  9 best               100          15   7.76µs   9.39µs   100908.        0B     20.2
#> 10 worst              100          15  13.92µs  15.53µs    61623.        0B     18.5
#> 11 best                 3         100   7.15µs    8.7µs   107326.        0B     32.2
#> 12 worst                3         100    7.9µs    9.5µs    98434.        0B     29.5
#> 13 best                 5         100    6.9µs   8.55µs   109002.        0B     32.7
#> 14 worst                5         100   7.59µs   8.76µs   108439.        0B     32.5
#> 15 best                10         100   6.71µs   7.48µs   129135.        0B     38.8
#> 16 worst               10         100   8.86µs   9.68µs   100360.        0B     30.1
#> 17 best                50         100   7.35µs   8.15µs   118349.        0B     35.5
#> 18 worst               50         100   16.5µs  17.36µs    56329.        0B     16.9
#> 19 best               100         100   7.71µs   8.44µs   114745.        0B     34.4
#> 20 worst              100         100  26.42µs  27.28µs    36033.        0B     10.8
```
