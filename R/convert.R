#' Convert an object from one type to another
#'
#' @description
#' `convert()` is a non-standard generic: it uses double dispatch on the first class of
#' `from` and `to` (and unlike normal dispatch, `to` is a class, not an object).
#'
#' `convert()` provides an automatic fallback if `from` inherits from `to`. You
#' can override this if you need some special behavior other than simply
#' stripping class.
#'
#' @param from An R7 object to convert.
#' @param to An R7 class specification, passed to [as_class()].
#' @param ... Other arguments passed to custom `convert()` methods.
#' @export
convert <- function(from, to, ...) {
  to <- as_class(to)
  check_can_inherit(to)

  dispatch <- list(obj_dispatch(from)[[1]], class_register(to))
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
