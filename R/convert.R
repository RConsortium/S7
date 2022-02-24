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
#' @param from An R7 object to convert.
#' @param to An R7 class specification, passed to [as_class()].
#' @param ... Other arguments passed to custom `convert()` methods.
#' @export
convert <- function(from, to, ...) {
  to <- as_class(to)
  check_can_inherit(to)

  dispatch <- list(obj_dispatch(from), class_register(to))
  convert <- .Call(method_, convert, dispatch, FALSE)

  if (!is.null(convert)) {
    convert(from, to, ...)
  } else if (class_inherits(from, to)) {
    from_class <- R7_class(from)
    if (is.null(from_class)) {
      from_props <- character()
    } else {
      from_props <- names(from_class@properties)
    }

    if (is_base_class(to)) {
      from <- zap_attr(from, c(from_props, "R7_class", "class"))
    } else if (is_S3_class(to)) {
      from <- zap_attr(from, c(from_props, "R7_class"))
      class(from) <- to$class
    } else if (is_class(to)) {
      from <- zap_attr(from, setdiff(from_props, names(to@properties)))
      attr(from, "R7_class") <- to
      class(from) <- setdiff(class_dispatch(to), "ANY")
    } else {
      stop("Unreachable")
    }
    from
  } else {
    method_lookup_error("convert", c("from", "to"), dispatch)
  }
}
# Converted to R7_generic on .onLoad
