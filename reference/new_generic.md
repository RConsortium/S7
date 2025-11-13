# Define a new generic

A generic function uses different implementations (*methods*) depending
on the class of one or more arguments (the *signature*). Create a new
generic with `new_generic()` then use method\<- to add methods to it.

Method dispatch is performed by `S7_dispatch()`, which must always be
included in the body of the generic, but in most cases `new_generic()`
will generate this for you.

Learn more in
[`vignette("generics-methods")`](https://rconsortium.github.io/S7/articles/generics-methods.md)

## Usage

``` r
new_generic(name, dispatch_args, fun = NULL)

S7_dispatch()
```

## Arguments

- name:

  The name of the generic. This should be the same as the object that
  you assign it to.

- dispatch_args:

  A character vector giving the names of one or more arguments used to
  find the method.

- fun:

  An optional specification of the generic, which must call
  `S7_dispatch()` to dispatch to methods. This is usually generated
  automatically from the `dispatch_args`, but you may want to supply it
  if you want to add additional required arguments, omit `...`, or
  perform some standardised computation in the generic.

  The `dispatch_args` must be the first arguments to `fun`, and, if
  present, `...` must immediately follow them.

## Value

An S7 generic, i.e. a function with class `S7_generic`.

## Dispatch arguments

The arguments that are used to pick the method are called the **dispatch
arguments**. In most cases, this will be one argument, in which case the
generic is said to use **single dispatch**. If it consists of more than
one argument, it's said to use **multiple dispatch**.

There are two restrictions on the dispatch arguments: they must be the
first arguments to the generic and if the generic uses `...`, it must
occur immediately after the dispatch arguments.

## See also

[`new_external_generic()`](https://rconsortium.github.io/S7/reference/new_external_generic.md)
to define a method for a generic in another package without taking a
strong dependency on it.

## Examples

``` r
# A simple generic with methods for some base types and S3 classes
type_of <- new_generic("type_of", dispatch_args = "x")
method(type_of, class_character) <- function(x, ...) "A character vector"
method(type_of, new_S3_class("data.frame")) <- function(x, ...) "A data frame"
method(type_of, class_function) <- function(x, ...) "A function"

type_of(mtcars)
#> [1] "A data frame"
type_of(letters)
#> [1] "A character vector"
type_of(mean)
#> [1] "A function"

# If you want to require that methods implement additional arguments,
# you can use a custom function:
mean2 <- new_generic("mean2", "x", function(x, ..., na.rm = FALSE) {
   S7_dispatch()
})

method(mean2, class_numeric) <- function(x, ..., na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  sum(x) / length(x)
}

# You'll be warned if you forget the argument:
method(mean2, class_character) <- function(x, ...) {
  stop("Not supported")
}
#> Warning: mean2(<character>) doesn't have argument `na.rm`
```
