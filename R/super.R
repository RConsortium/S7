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
#' @param from An S7 object to cast.
#' @param to An S7 class specification, passed to [as_class()]. Must be a
#'   superclass of `object`.
#' @returns An `S7_super` object which should always be passed
#'   immediately to a generic. It has no other special behavior.
#' @export
#' @examples
#' foo1 <- new_class("foo1", properties = list(x = class_numeric, y = class_numeric))
#' foo2 <- new_class("foo2", foo1, properties = list(z = class_numeric))
#'
#' total <- new_generic("total", "x")
#' method(total, foo1) <- function(x) x@x + x@y
#'
#' # This won't work because it'll be stuck in an infinite loop:
#' method(total, foo2) <- function(x) total(x) + x@z
#'
#' # We could write
#' method(total, foo2) <- function(x) x@x + x@y + x@z
#' # but then we'd need to remember to update it if the implementation
#' # for total(<foo1>) ever changed.
#'
#' # So instead we use `super()` to call the method for the parent class:
#' method(total, foo2) <- function(x) total(super(x, to = foo1)) + x@z
#' total(foo2(1, 2, 3))
#'
#' # To see the difference between convert() and super() we need a
#' # method that calls another generic
#'
#' bar1 <- new_generic("bar1", "x")
#' method(bar1, foo1) <- function(x) 1
#' method(bar1, foo2) <- function(x) 2
#'
#' bar2 <- new_generic("bar2", "x")
#' method(bar2, foo1) <- function(x) c(1, bar1(x))
#' method(bar2, foo2) <- function(x) c(2, bar1(x))
#'
#' obj <- foo2(1, 2, 3)
#' bar2(obj)
#' # convert() affects every generic:
#' bar2(convert(obj, to = foo1))
#' # super() only affects the _next_ call to a generic:
#' bar2(super(obj, to = foo1))
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
