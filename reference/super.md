# Force method dispatch to use a superclass

`super(from, to)` causes the dispatch for the next generic to use the
method for the superclass `to` instead of the actual class of `from`.
It's needed when you want to implement a method in terms of the
implementation of its superclass.

### S3 & S4

`super()` performs a similar role to
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) in S3 or
[`methods::callNextMethod()`](https://rdrr.io/r/methods/NextMethod.html)
in S4, but is much more explicit:

- The super class that `super()` will use is known when write `super()`
  (i.e. statically) as opposed to when the generic is called (i.e.
  dynamically).

- All arguments to the generic are explicit; they are not automatically
  passed along.

This makes `super()` more verbose, but substantially easier to
understand and reason about.

### `super()` in S3 generics

Note that you can't use `super()` in methods for an S3 generic. For
example, imagine that you have made a subclass of "integer":

    MyInt <- new_class("MyInt", parent = class_integer, package = NULL)

Now you go to write a custom print method:

    method(print, MyInt) <- function(x, ...) {
       cat("<MyInt>")
       print(super(x, to = class_integer))
    }

    MyInt(10L)
    #> <MyInt>super(<MyInt>, <integer>)

This doesn't work because [`print()`](https://rdrr.io/r/base/print.html)
isn't an S7 generic so doesn't understand how to interpret the special
object that `super()` produces. While you could resolve this problem
with [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) (because S7
is implemented on top of S3), we instead recommend using
[`S7_data()`](https://rconsortium.github.io/S7/reference/S7_data.md) to
extract the underlying base object:

    method(print, MyInt) <- function(x, ...) {
       cat("<MyInt>")
       print(S7_data(x))
    }

    MyInt(10L)
    #> <MyInt>[1] 10

## Usage

``` r
super(from, to)
```

## Arguments

- from:

  An S7 object to cast.

- to:

  An S7 class specification, passed to
  [`as_class()`](https://rconsortium.github.io/S7/reference/as_class.md).
  Must be a superclass of `object`.

## Value

An `S7_super` object which should always be passed immediately to a
generic. It has no other special behavior.

## Examples

``` r
Foo1 <- new_class("Foo1", properties = list(x = class_numeric, y = class_numeric))
Foo2 <- new_class("Foo2", Foo1, properties = list(z = class_numeric))

total <- new_generic("total", "x")
method(total, Foo1) <- function(x) x@x + x@y

# This won't work because it'll be stuck in an infinite loop:
method(total, Foo2) <- function(x) total(x) + x@z

# We could write
method(total, Foo2) <- function(x) x@x + x@y + x@z
#> Overwriting method total(<Foo2>)
# but then we'd need to remember to update it if the implementation
# for total(<Foo1>) ever changed.

# So instead we use `super()` to call the method for the parent class:
method(total, Foo2) <- function(x) total(super(x, to = Foo1)) + x@z
#> Overwriting method total(<Foo2>)
total(Foo2(1, 2, 3))
#> [1] 6

# To see the difference between convert() and super() we need a
# method that calls another generic

bar1 <- new_generic("bar1", "x")
method(bar1, Foo1) <- function(x) 1
method(bar1, Foo2) <- function(x) 2

bar2 <- new_generic("bar2", "x")
method(bar2, Foo1) <- function(x) c(1, bar1(x))
method(bar2, Foo2) <- function(x) c(2, bar1(x))

obj <- Foo2(1, 2, 3)
bar2(obj)
#> [1] 2 2
# convert() affects every generic:
bar2(convert(obj, to = Foo1))
#> [1] 1 1
# super() only affects the _next_ call to a generic:
bar2(super(obj, to = Foo1))
#> [1] 1 2
```
