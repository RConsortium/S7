#' Does this object inherit from an S7 class?
#'
#' * `S7_inherits()` returns `TRUE` or `FALSE`.
#' * `check_is_S7()` throws an error.
#'
#' @param x An object
#' @param class An S7 class. Can be omitted in `check_is_S7()`.
#' @param arg Argument name used in error message.
#' @returns `S7_inherits()` returns a single `TRUE` or `FALSE`;
#'   `check_is_S7()` returns nothing; it's called for its side-effects.
#' @export
#' @examples
#' foo1 <- new_class("foo1")
#' foo2 <- new_class("foo2")
#'
#' S7_inherits(foo1(), foo1)
#' check_is_S7(foo1())
#' check_is_S7(foo1(), foo1)
#'
#' S7_inherits(foo1(), foo2)
#' try(check_is_S7(foo1(), foo2))
S7_inherits <- function(x, class) {
  if (!inherits(class, "S7_class")) {
    stop("`class` is not an <S7_class>")
  }

  inherits(x, "S7_object") && inherits(x, S7_class_name(class))
}

#' @export
#' @rdname S7_inherits
check_is_S7 <- function(x, class = NULL, arg = deparse(substitute(x))) {
  if (is.null(class)) {
    if (!inherits(x, "S7_object")) {
      msg <- sprintf("`%s` must be an <S7_object>, not a %s", arg, obj_desc(x))
      stop(msg, call. = FALSE)
    }
  } else {
    if (!S7_inherits(x, class)) {
      msg <- sprintf("`%s` must be a %s, not a %s", arg, class_desc(class), obj_desc(x))
      stop(msg, call. = FALSE)
    }
  }

  invisible()
}
