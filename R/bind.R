#' Create and name an object in one step
#'
#' @description
#' Functions like [new_class()] and [new_generic()] take a `name` that, by
#' convention, matches the name of the variable that you assign their result
#' to. The `:=` operator eliminates this duplication: `Foo := new_class()` is
#' equivalent to `Foo <- new_class(name = "Foo")`.
#'
#' `:=` works with any function that has a `name` argument, but bear in mind
#' that it adds `name` to the call as a named argument, so any other arguments
#' supplied positionally will shift to fill the remaining parameters.
#'
#' @usage lhs := rhs
#' @param lhs A bare symbol: both the name of the variable to create and the
#'   `name` supplied to the right-hand side.
#' @param rhs A call to a function with a `name` argument.
#' @return The result of evaluating `rhs`, which is also assigned to `lhs` in
#'   the calling environment, returned invisibly.
#' @export
#' @rdname named-bind
#' @examples
#' Range := new_class(properties = list(
#'   start = class_double,
#'   end = class_double
#' ))
#' Range
#'
#' describe := new_generic("x")
#' describe
`:=` <- function(lhs, rhs) {
  cl <- sys.call()
  if (!is.symbol(cl[[2L]])) {
    stop2("Left-hand side of `:=` must be a symbol.")
  }
  if (!is.call(cl[[3L]])) {
    stop2("Right-hand side of `:=` must be a function call.")
  }
  if ("name" %in% names(cl[[3L]])) {
    stop2("Right-hand side of `:=` must not already supply a `name` argument.")
  }

  cl[[1L]] <- quote(`<-`)
  cl[[3L]]$name <- as.character(cl[[2L]])
  invisible(eval.parent(cl))
}
