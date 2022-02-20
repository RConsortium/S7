#' Cast an object from one type to another
#'
#' @description
#' `cast()` is a non-standard generic: it dispatch on the terminal class of
#' `from` and `to` (and unlike normal dispatch, `to` is a class, not an object).
#'
#' `cast()` provides an automatic fallback if `from` inherits from `to`. You
#' can override this if you need some special behavior other than simply
#' stripping class.
#'
#' @param from An R7 object to cast.
#' @param to An R7 class specification, passed to [as_class()].
#' @param ... Other arguments passed to custom `cast()` methods.
#' @export
cast <- function(from, to, ...) {
  to <- as_class(to)
  check_can_inherit(to)

  dispatch <- list(obj_dispatch(from)[[1]], class_register(to))
  cast <- .Call(method_, cast, dispatch, FALSE)

  if (!is.null(cast)) {
    cast(from, to, ...)
  } else if (class_inherits(from, to)) {
    if (is_base_class(to)) {
      R7_data(from)
    } else if (is_class(to)) {
      attr(from, "object_class") <- to
      class(from) <- class_dispatch(to)
      from
    } else if (is_S3_class(to)) {
      class(from) <- to$class
      from
    } else {
      stop("Unreachable")
    }
  } else {
    method_lookup_error("cast", c("from", "to"), dispatch)
  }
}
# Converted to R7_generic on .onLoad

#' Force R7 method dispatch to use a superclass
#'
#' @description
#' `up_cast()` is a variant of `cast()` that avoids creating a copy of `from`
#' when you want to re-call a generic, forcing method dispatch to find an
#' implementation for a superclass.
#'
#' `cast()` must create a copy because it changes the class attribute of `from`.
#' In most cases this copy will be cheap, because the properties will be copied
#' by reference (and only duplicated on modification). It might be expensive,
#' however, if the underlying data is a large vector or if you have a very
#' large number of properties. `up_cast()` avoids the copy, but only works
#' for method dispatch.
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
#' @param from An R7 object to cast.
#' @param to An R7 class specification, passed to [as_class()]. Must be a
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
up_cast <- function(from, to = NULL) {
  check_R7(from)

  if (is.null(to)) {
    to <- object_class(from)@parent
    if (is.null(to)) {
      stop("R7_object has no parent class")
    }
  } else {
    to <- as_class(to)
    check_can_inherit(to)
    if (!class_inherits(from, to)) {
      msg <- sprintf("`object` %s does not inherit from %s", obj_desc(from), class_desc(to))
      stop(msg)
    }
  }

  # Must not change order of these fields as C code indexes by position
  structure(
    list(
      object = from,
      dispatch = class_register(to)
    ),
    class = "R7_up_class"
  )
}

is_up_cast <- function(x) inherits(x, "R7_up_class")
