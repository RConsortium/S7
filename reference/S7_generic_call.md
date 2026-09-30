# Access the generic call and user frame from within a method

These helpers give a method stable access to three pieces of context
that are otherwise obscured by S7's dispatch machinery:

- `S7_generic_call()` returns the call to the generic. This is useful as
  the `call` for an error message, so that the user sees the generic
  call (e.g. `foo(1)`) rather than S7's internal dispatch.

- `S7_user_frame()` returns the frame from which the generic was called.
  This is the equivalent of
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) in an S3
  method, and is useful if you need non-standard evaluation.

- `S7_generic_fun()` returns the generic function itself. This is useful
  if you need to inspect the generic, e.g. to retrieve its name or
  dispatch arguments.

By default, `S7_generic_call()` and `S7_user_frame()` report the nearest
call to the generic and its caller. Set `skip = "super"` to skip
intermediate frames when a method re-dispatches the same generic to a
superclass with
[`super()`](https://rconsortium.github.io/S7/reference/super.md),
reporting the outermost user-facing call and its caller.

You can also call these helpers from a function that the method calls,
such as a shared error helper; they use the innermost active method of
an S7 generic. They aren't supported in methods for S3 or S4 generics,
or for operators like `+`.

## Usage

``` r
S7_generic_call(match = FALSE, skip = c("none", "super"))

S7_user_frame(skip = c("none", "super"))

S7_generic_fun()
```

## Arguments

- match:

  Set to `TRUE` to process with
  [`match.call()`](https://rdrr.io/r/base/match.call.html) and name all
  arguments.

- skip:

  Whether to skip calls that re-dispatch the same generic with
  [`super()`](https://rconsortium.github.io/S7/reference/super.md). The
  default, `"none"`, reports the nearest generic call; `"super"` skips
  past these re-dispatches.

## Value

`S7_generic_call()` returns a call; `S7_user_frame()` returns an
environment; `S7_generic_fun()` returns the generic function. All error
if called outside of a method.

## See also

[`S7_dispatch()`](https://rconsortium.github.io/S7/reference/new_generic.md)
for how methods are called, and
[`super()`](https://rconsortium.github.io/S7/reference/super.md) for
superclass dispatch.

## Examples

``` r
# S7_generic_call() reports the call to the generic:
foo := new_generic("x")
method(foo, class_double) <- function(x) {
  list(nearest = S7_generic_call(), user = S7_generic_call(skip = "super"))
}
foo(1)
#> $nearest
#> foo(1)
#> 
#> $user
#> foo(1)
#> 

# Set skip = "super" to skip past super() re-dispatches:
Number := new_class(parent = class_double)
method(foo, Number) <- function(x) {
  foo(super(x, class_double))
}
foo(Number(1))
#> $nearest
#> foo(super(x, class_double))
#> 
#> $user
#> foo(Number(1))
#> 

# S7_user_frame() supplies the enclosing environment for non-standard
# evaluation, so an expression can mix columns of the data with variables
# from where the generic was called, like subset():
keep_rows := new_generic("data")
method(keep_rows, class_data.frame) <- function(data, condition) {
  rows <- eval(substitute(condition), data, S7_user_frame())
  data[rows, , drop = FALSE]
}

threshold <- 4
df <- data.frame(x = 1:3, y = c(2, 5, 8))
# `x` and `y` come from the data frame; `threshold` from this frame
keep_rows(df, x + y > threshold)
#>   x y
#> 2 2 5
#> 3 3 8

# S7_generic_fun() returns the generic itself, e.g. to use its name in a
# message:
bar := new_generic("x")
method(bar, class_double) <- function(x) {
  generic <- S7_generic_fun()
  paste0("Called ", generic@name, "()")
}
bar(1)
#> [1] "Called bar()"
```
