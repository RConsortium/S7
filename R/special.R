#' Special dispatch types
#'
#' * Use `missing_class` when you the user has not supplied an argument
#' * Use `any_class` for a default method that is called only if no other
#'   methods are matched
#'
#' @export
#' @examples
#' foo <- new_generic("foo", "x")
#' method(foo, integer) <- function(x) "integer"
#' method(foo, missing_class) <- function(x) "missing"
#' method(foo, any_class) <- function(x) "fallback"
#'
#' foo(1)
#' foo()
#' foo("x")
missing_class <- structure(list(), class = "R7_missing")

is_missing_class <- function(x) inherits(x, "R7_missing")

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
#' @rdname missing_class
any_class <- structure(list(), class = "R7_any")

is_any_class <- function(x) inherits(x, "R7_any")

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
