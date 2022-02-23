#' Force method dispatch to use a superclass
#'
#' @description
#' `super()` causes the dispatch for the next generic to use the method for
#' the specified superclass. It is useful when you want to implement a method
#' in terms of the implementation of its superclass.
#'
#' # Compared to S3 and S4
#' `super()` performs a similar role to [NextMethod()] in S3 or
#' [methods::callNextMethod()] in S4, but is much more explicit:
#'
#' * The class that `super()` will dispatch to is known at the time you
#'   write `super()`, not only when it's called.
#' * All arguments to the generic are explicit; they are not automatically
#'   passed along.
#'
#' This makes `super()` more verbose, but substantially easier to
#' understand and reason about.
#'
#' @param from An R7 object to cast.
#' @param to An R7 class specification, passed to [as_class()]. Must be a
#'   superclass of `object`.
#' @returns An `R7_super` object which should always be passed
#'   immediately to a generic. It has no other special behavior.
#' @export
#' @examples
#' foo1 <- new_class("foo1", properties = list(x = "numeric", y = "numeric"))
#' foo2 <- new_class("foo2", foo1, properties = list(z = "numeric"))
#'
#' total <- new_generic("total", "x")
#' method(total, foo1) <- function(x) x@x + x@y
#'
#' # This doesn't work because it'll be stuck in an infinite loop:
#' method(total, foo2) <- function(x) total(x) + x@z
#'
#' # So instead we use `super()` to call the method for the parent class:
#' method(total, foo2) <- function(x) total(super(x, foo1)) + x@z
#' total(foo2(1, 2, 3))
#'
#' # To see the difference between cast() and super() we need a
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
#' # cast() affects every generic:
#' bar2(cast(obj, foo1))
#' # super() only affects the _next_ generic:
#' bar2(super(obj, foo1))
super <- function(from, to) {
  check_R7(from)

  to <- as_class(to)
  check_can_inherit(to)
  if (!class_inherits(from, to)) {
    msg <- sprintf(
      "Can't cast: %s doesn't inherit from %s",
      obj_desc(from),
      class_desc(to)
    )
    stop(msg)
  }

  # Must not change order of these fields as C code indexes by position
  structure(
    list(
      object = from,
      dispatch = class_register(to)
    ),
    class = "R7_super"
  )
}
