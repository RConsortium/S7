#' Force method dispatch to use a superclass
#'
#' @description
#' `super(from, to)` causes the dispatch for the next generic to use the method
#' for the superclass `to` instead of the actual class of `from`. It's needed
#' when you want to implement a method in terms of the implementation of its
#' superclass.
#'
#' ## S3 & S4
#' `super()` performs a similar role to [NextMethod()] in S3 or
#' [methods::callNextMethod()] in S4, but is much more explicit:
#'
#' * The super class that `super()` will use is known when write `super()`
#'   (i.e. statically) as opposed to when the generic is called
#'   (i.e. dynamically).
#'
#' * All arguments to the generic are explicit; they are not automatically
#'   passed along.
#'
#' This makes `super()` more verbose, but substantially easier to
#' understand and reason about.
#'
#' ## `super()` in S3 generics
#'
#' Note that you can't use `super()` in methods for an S3 generic.
#' For example, imagine that you have made a subclass of "integer":
#'
#' ```{r}
#' MyInt <- new_class("MyInt", parent = class_integer, package = NULL)
#' ```
#'
#' Now you go to write a custom print method:
#'
#' ```{r}
#' method(print, MyInt) <- function(x, ...) {
#'    cat("<MyInt>")
#'    print(super(x, to = class_integer))
#' }
#'
#' MyInt(10L)
#' ```
#'
#' This doesn't work because `print()` isn't an S7 generic so doesn't
#' understand how to interpret the special object that `super()` produces.
#' While you could resolve this problem with [NextMethod()] (because S7 is
#' implemented on top of S3), we instead recommend using [S7_data()] to extract
#' the underlying base object:
#'
#' ```{r}
#' method(print, MyInt) <- function(x, ...) {
#'    cat("<MyInt>")
#'    print(S7_data(x))
#' }
#'
#' MyInt(10L)
#' ```
#'
#' @param from An S7 object to cast.
#' @param to An S7 class specification, passed to [as_class()]. Must be a
#'   superclass of `object`.
#' @returns An `S7_super` object which should always be passed
#'   immediately to a generic. It has no other special behavior.
#' @export
#' @examples
#' Foo1 <- new_class("Foo1", properties = list(x = class_numeric, y = class_numeric))
#' Foo2 <- new_class("Foo2", Foo1, properties = list(z = class_numeric))
#'
#' total <- new_generic("total", "x")
#' method(total, Foo1) <- function(x) x@x + x@y
#'
#' # This won't work because it'll be stuck in an infinite loop:
#' method(total, Foo2) <- function(x) total(x) + x@z
#'
#' # We could write
#' method(total, Foo2) <- function(x) x@x + x@y + x@z
#' # but then we'd need to remember to update it if the implementation
#' # for total(<Foo1>) ever changed.
#'
#' # So instead we use `super()` to call the method for the parent class:
#' method(total, Foo2) <- function(x) total(super(x, to = Foo1)) + x@z
#' total(Foo2(1, 2, 3))
#'
#' # To see the difference between convert() and super() we need a
#' # method that calls another generic
#'
#' bar1 <- new_generic("bar1", "x")
#' method(bar1, Foo1) <- function(x) 1
#' method(bar1, Foo2) <- function(x) 2
#'
#' bar2 <- new_generic("bar2", "x")
#' method(bar2, Foo1) <- function(x) c(1, bar1(x))
#' method(bar2, Foo2) <- function(x) c(2, bar1(x))
#'
#' obj <- Foo2(1, 2, 3)
#' bar2(obj)
#' # convert() affects every generic:
#' bar2(convert(obj, to = Foo1))
#' # super() only affects the _next_ call to a generic:
#' bar2(super(obj, to = Foo1))
super <- function(from, to) {
  check_is_S7(from)

  to <- as_class(to)
  check_can_inherit(to)
  if (!class_inherits(from, to)) {
    msg <- sprintf(
      "%s doesn't inherit from %s",
      obj_desc(from),
      class_desc(to)
    )
    stop(msg)
  }

  # Must not change order of these fields as C code indexes by position
  structure(
    list(
      object = from,
      dispatch = class_dispatch(to)
    ),
    class = "S7_super"
  )
}

#' @export
print.S7_super <- function(x, ...) {
  str(x, ...)
  invisible(x)
}
#' @export
str.S7_super <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  cat("super(", obj_desc(object$object), ", <", object$dispatch[[1]], ">)", sep = "")
}
