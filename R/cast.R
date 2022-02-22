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
      class(from) <- setdiff(class_dispatch(to), "ANY")
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


#' Force one method dispatch to use a superclass
#'
#' @description
#' `super()` is a variant of `cast()` that only affects the dispatch
#' for single generic. It is useful when you want to re-call a generic,
#' forcing method dispatch to find an implementation for a superclass.
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
#'   superclass of `object`. If not specified, defaults to the parent of
#'   `from`.
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
#' # This ensures the nested total calls the parent method
#' method(total, foo2) <- function(x) total(super(x)) + x@z
#'
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
super <- function(from, to = NULL) {
  check_R7(from)

  if (is.null(to)) {
    from_class <- object_class(from)
    if (is.null(from_class)) {
      stop("Can't cast: R7_object has no parent class")
    }
    to <- from_class@parent
  } else {
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
