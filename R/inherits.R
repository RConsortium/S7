#' Does this object inherit from an S7 class?
#'
#' * `S7_inherits()` returns `TRUE` or `FALSE`.
#' * `check_is_S7()` throws an error if `x` isn't the specified `class`.
#'
#' @param x An object
#' @param class An S7 class or `NULL`. If `NULL`, tests whether `x` is an
#'   S7 object without testing for a specific class.
#' @param arg Argument name used in error message.
#' @returns
#' * `S7_inherits()` returns a single `TRUE` or `FALSE`.
#' * `check_is_S7()` returns nothing; it's called for its side-effects.
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
S7_inherits <- function(x, class = NULL) {
  if (!(is.null(class) || inherits(class, "S7_class"))) {
    stop("`class` must be an <S7_class> or NULL")
  }

  inherits(x, "S7_object") &&
    (is.null(class) || inherits(x, S7_class_name(class)))
}

#' @export
#' @rdname S7_inherits
check_is_S7 <- function(x, class = NULL, arg = deparse(substitute(x))) {
  if (S7_inherits(x, class)) {
    return(invisible())
  }

  msg <- sprintf(
    "`%s` must be %s, not a %s",
    arg,
    if (is.null(class)) "an <S7_object>" else paste0("a ", class_desc(class)),
    obj_desc(x)
  )
  stop(msg, call. = FALSE)
}
