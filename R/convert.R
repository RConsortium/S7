#' Convert an object from one type to another
#'
#' @description
#' `convert()` uses double-dispatch, because conversion depends on both `from`
#' and `to`. The dispatch is non-standard, because `to` is a class (not an
#' object), and it does not take advantage of inheritance (because if you
#' convert `x` to `superFoo` you shouldn't get an instance of `Foo` back).
#'
#' `convert()` provides built-in implementations if `from` inherits from `to`.
#' This default strips any properties that `from` possesses that `to` does not,
#' and resets the class.
#'
#' @param from An S7 object to convert.
#' @param to An S7 class specification, passed to [as_class()].
#' @param ... Other arguments passed to custom `convert()` methods.
#' @export
#' @examples
#' foo1 <- new_class("foo1", properties = list(x = class_integer))
#' foo2 <- new_class("foo2", foo1, properties = list(y = class_double))
#'
#' method(convert, list(foo1, class_integer)) <- function(from, to) from@x
#' method(convert, list(foo2, class_double)) <- function(from, to) from@y
#'
#' convert(foo1(x = 1L), to = class_integer)
#' try(convert(foo1(x = 1L), to = class_double))
#'
#' convert(foo2(x = 1L, y = 2), to = class_integer)
#' convert(foo2(x = 1L, y = 2), to = class_double)
#' convert(foo2(x = 1L, y = 2), to = foo1)
#'
#' # If we define a convert method for integer + foo1:
#' method(convert, list(class_integer, foo1)) <- function(from, to) foo1(x = from)
#' convert(1L, to = foo1)
#' # Converting too foo2 still errors
#' try(convert(1L, to = foo2))
convert <- function(from, to, ...) {
  to <- as_class(to)
  check_can_inherit(to)

  dispatch <- list(obj_dispatch(from), class_register(to))
  convert <- .Call(method_, convert, dispatch, environment(), FALSE)

  if (!is.null(convert)) {
    convert(from, to, ...)
  } else if (class_inherits(from, to)) {
    from_class <- S7_class(from)
    if (is.null(from_class)) {
      from_props <- character()
    } else {
      from_props <- names(from_class@properties)
    }

    if (is_base_class(to)) {
      from <- zap_attr(from, c(from_props, "S7_class", "class"))
    } else if (is_S3_class(to)) {
      from <- zap_attr(from, c(from_props, "S7_class"))
      class(from) <- to$class
    } else if (is_class(to)) {
      from <- zap_attr(from, setdiff(from_props, names(to@properties)))
      attr(from, "S7_class") <- to
      class(from) <- class_dispatch(to)
    } else {
      stop("Unreachable")
    }
    from
  } else {
    msg <- paste0(
      "Can't find method for generic `convert()` with dispatch classes:\n",
      "- from: ", obj_desc(from), "\n",
      "- to  : ", class_desc(to), "\n"
    )
    stop(msg, call. = FALSE)
  }
}
# Converted to S7_generic on .onLoad
