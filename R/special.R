#' Dispatch on a missing argument
#'
#' Use `class_missing` to dispatch when the user has not supplied an argument,
#' i.e. it's missing in the sense of [missing()], not in the sense of
#' [is.na()].
#'
#' @export
#' @return Sentinel objects used for special types of dispatch.
#' @format NULL
#' @examples
#' foo <- new_generic("foo", "x")
#' method(foo, class_numeric) <- function(x) "number"
#' method(foo, class_missing) <- function(x) "missing"
#' method(foo, class_any) <- function(x) "fallback"
#'
#' foo(1)
#' foo()
#' foo("")
class_missing <- structure(list(), class = "S7_missing")

is_class_missing <- function(x) inherits(x, "S7_missing")

#' @export
print.S7_missing <- function(x, ...) {
  cat("<S7_missing>\n")
  invisible(x)
}
#' @export
str.S7_missing <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object)
}

#' Dispatch on any class
#'
#' Use `class_any` to register a default method that is called when no other
#' methods are matched.
#'
#' @export
#' @format NULL
#' @examples
#' foo <- new_generic("foo", "x")
#' method(foo, class_numeric) <- function(x) "number"
#' method(foo, class_any) <- function(x) "fallback"
#'
#' foo(1)
#' foo("x")
class_any <- structure(list(), class = "S7_any")

is_class_any <- function(x) inherits(x, "S7_any")

#' @export
print.S7_any <- function(x, ...) {
  cat("<S7_any>\n")
  invisible(x)
}
#' @export
str.S7_any <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object)
}
