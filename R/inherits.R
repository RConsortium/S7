#' Does this object inherit from an R7 class?
#'
#' * `R7_inherits()` returns `TRUE` or `FALSE`
#' * `check_R7_inherits()` throws an error.
#'
#' @param x An object
#' @param class An R7 class
#' @param arg Argument name used in error message.
#' @export
#' @examples
#' foo1 <- new_class("foo1")
#' foo2 <- new_class("foo2")
#'
#' R7_inherits(foo1(), foo1)
#' check_R7_inherits(foo1(), foo1)
#'
#' R7_inherits(foo1(), foo2)
#' try(check_R7_inherits(foo1(), foo2))
R7_inherits <- function(x, class) {
  if (!inherits(class, "R7_class")) {
    stop("`class` is not an <R7_class>")
  }

  inherits(x, "R7_object") && inherits(x, R7_class_name(class))
}

#' @export
#' @rdname R7_inherits
check_R7_inherits <- function(x, class, arg = deparse(substitute(x))) {
  if (!R7_inherits(x, class)) {
    stop(sprintf("`%s` is not a %s", arg, class_desc(class)), call. = FALSE)
  }
  invisible()
}
