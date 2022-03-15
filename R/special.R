#' Special dispatch types
#'
#' * Use `class_missing` when the user has not supplied an argument
#' * Use `class_any` for a default method that is called only if no other
#'   methods are matched
#'
#' @export
#' @examples
#' foo <- new_generic("foo", "x")
#' method(foo, class_integer) <- function(x) "integer"
#' method(foo, class_missing) <- function(x) "missing"
#' method(foo, class_any) <- function(x) "fallback"
#'
#' foo(1)
#' foo()
#' foo("x")
class_missing <- structure(list(), class = "R7_missing")

is_class_missing <- function(x) inherits(x, "R7_missing")

#' @export
print.R7_missing <- function(x, ...) {
  cat("<R7_missing>\n")
  invisible(x)
}
#' @export
str.R7_missing <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object)
}

#' @export
#' @rdname class_missing
class_any <- structure(list(), class = "R7_any")

is_class_any <- function(x) inherits(x, "R7_any")

#' @export
print.R7_any <- function(x, ...) {
  cat("<R7_any>\n")
  invisible(x)
}
#' @export
str.R7_any <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object)
}
