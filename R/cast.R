#' Force method dispatch to use a superclass
#'
#' @description
#' When implementing a method, it's often useful to borrow some of the
#' implementation from the method for a superclass. `up_cast()` lets you
#' re-call a generic, forcing method dispatch to use the method for a parent,
#' without modifying (and hence making a copy) of the object that you're
#' working with.
#'
#' # Compared to S3 and S4
#' `up_cast()` performs a similar role to [NextMethod()] in S3 or
#' [methods::callNextMethod()] in S4. It has two main differences:
#'
#' * When you call `up_cast()` you must specify which parent method you
#'   want to call.
#' * You must manually pass along all of the arguments.
#'
#' This makes `up_cast()` more verbose, but substantially easier to reason
#' about. It also avoids some of the dynamism of `nextMethod()`: registering
#' methods for a parent class can not method dispatch for a child class.
#'
#' @param object An R7 object
#' @param class An R7 class specification, passed to [as_class()]. Must be a
#'   superclass of `object`. If not specified, defaults to the parent of
#'   `object`.
#' @returns An `R7_up_class` object which should always be passed
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
#' # This ensures the nested total calls the parent method
#' method(total, foo2) <- function(x) total(up_cast(x)) + x@z
#'
#' total(foo2(1, 2, 3))
up_cast <- function(object, class = NULL) {
  check_R7(object)

  if (is.null(class)) {
    class <- object_class(object)@parent
    if (is.null(class)) {
      stop("R7_object has no parent class")
    }
  } else {
    class <- as_class(class)
    check_can_inherit(class)
    if (!class_inherits(object, class)) {
      msg <- sprintf("`object` %s does not inherit from %s", obj_desc(object), class_desc(class))
      stop(msg)
    }
  }

  # Must not change order of these fields as C code indexes by position
  structure(
    list(
      object = object,
      dispatch = class_register(class)
    ),
    class = "R7_up_class"
  )
}

is_up_cast <- function(x) inherits(x, "R7_up_class")
