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
#> 1 foo_S7(x)    5.74µs   7.23µs   127865.    10.8KB     38.4
#> 2 foo_S3(x)    1.87µs    2.1µs   427247.        0B     42.7
#> 3 foo_S4(x)    1.87µs   2.29µs   410068.        0B     41.0

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
#> 1 bar_S7(x, y)   9.72µs  11.45µs    84654.        0B     42.3
#> 2 bar_S4(x, y)    4.9µs   5.69µs   172217.        0B     34.5
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
#>  1 best                 3          15   5.76µs   7.21µs   133421.        0B     40.0
#>  2 worst                3          15    6.4µs   7.05µs   137990.        0B     41.4
#>  3 best                 5          15   5.76µs   6.77µs   144446.        0B     43.3
#>  4 worst                5          15   5.94µs   6.88µs   142382.        0B     42.7
#>  5 best                10          15   5.74µs   6.69µs   146494.        0B     44.0
#>  6 worst               10          15   6.08µs   7.09µs   138613.        0B     41.6
#>  7 best                50          15   5.89µs   6.93µs   139431.        0B     41.8
#>  8 worst               50          15   7.41µs   8.71µs   111851.        0B     33.6
#>  9 best               100          15    6.1µs   7.43µs   131323.        0B     39.4
#> 10 worst              100          15   8.81µs  10.26µs    96205.        0B     28.9
#> 11 best                 3         100   5.84µs   6.98µs   139946.        0B     42.0
#> 12 worst                3         100   5.94µs   7.58µs   128775.        0B     38.6
#> 13 best                 5         100   5.85µs   6.84µs   139444.        0B     55.8
#> 14 worst                5         100   6.15µs   7.14µs   137967.        0B     41.4
#> 15 best                10         100   5.77µs   6.81µs   142693.        0B     42.8
#> 16 worst               10         100   6.29µs   7.53µs   130370.        0B     39.1
#> 17 best                50         100    5.7µs   6.81µs   144059.        0B     43.2
#> 18 worst               50         100  10.08µs  11.72µs    83775.        0B     25.1
#> 19 best               100         100   6.15µs   7.21µs   133294.        0B     40.0
#> 20 worst              100         100  14.43µs  17.39µs    55287.        0B     16.6
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
#>  1 best                 3          15    7.2µs   8.55µs   114255.        0B     45.7
#>  2 worst                3          15   7.45µs   8.67µs   113678.        0B     45.5
#>  3 best                 5          15   7.24µs   8.53µs   114368.        0B     45.8
#>  4 worst                5          15   7.62µs   8.99µs   108889.        0B     43.6
#>  5 best                10          15   7.13µs   7.84µs   121593.        0B     36.5
#>  6 worst               10          15   7.89µs   8.51µs   113501.        0B     45.4
#>  7 best                50          15   7.61µs   8.67µs   110910.        0B     44.4
#>  8 worst               50          15   10.4µs  12.06µs    82296.        0B     32.9
#>  9 best               100          15   8.69µs   9.45µs   103017.        0B     41.2
#> 10 worst              100          15  13.57µs  16.18µs    61304.        0B     24.5
#> 11 best                 3         100   7.48µs   8.57µs   114055.        0B     45.6
#> 12 worst                3         100   8.17µs   9.29µs   106139.        0B     42.5
#> 13 best                 5         100   7.36µs   8.48µs   116083.        0B     46.5
#> 14 worst                5         100   8.22µs   9.32µs   105684.        0B     42.3
#> 15 best                10         100   7.28µs   8.44µs   115557.        0B     46.2
#> 16 worst               10         100   9.23µs   10.7µs    91193.        0B     36.5
#> 17 best                50         100   7.84µs   9.08µs   106808.        0B     42.7
#> 18 worst               50         100  15.24µs  17.39µs    56711.        0B     22.7
#> 19 best               100         100   8.02µs   9.36µs   103460.        0B     51.8
#> 20 worst              100         100  23.45µs     27µs    36409.        0B     18.2
```
