#' Define a new generic
#'
#' @description
#' A generic function uses different implementations depending on the class
#' of one or more arguments (the `signature`). Create a new generic with
#' `new_generic()` then use [method<-] to add methods to it.
#'
#' @param name The name of the generic. This should be the same as the object
#'   that you assign it to.
#' @param signature A character vector providing the names of arguments to
#'   dispatch on. If omitted, defaults to the required arguments of `fun`.
#' @param fun An optional specification of the generic, which must call
#'  `method_call()` to dispatch to methods. This is usually generated
#'  automatically from the `signature`, but you may want to supply it if
#'  you want to add additional required arguments, or perform some standardised
#'  computation in the generic.
#' @seealso [new_external_generic()] to define a method for a generic
#'  in another package without taking a strong dependency on it.
#' @export
#' @examples
#' # A simple generic with methods for some base types and S3 classes
#' type_of <- new_generic("type_of", signature = "x")
#' method(type_of, "character") <- function(x, ...) "A character vector"
#' method(type_of, "data.frame") <- function(x, ...) "A data frame"
#' method(type_of, "function") <- function(x, ...) "A function"
#'
#' type_of(mtcars)
#' type_of(letters)
#' type_of(mean)
#'
#' # If you want to require methods implement additional arguments, supply
#' # them after ... in the call
#' mean2 <- new_generic("mean2", function(x, ..., na.rm = TRUE) {
#'    method_call()
#' })
#' method(mean2, "numeric") <- function(x, ..., na.rm = TRUE) {
#'   if (na.rm) {
#'     x <- x[!is.na(x)]
#'   }
#'   sum(x) / length(x)
#' }
#' method(mean2, "character") <- function(x, ...) {stop("Not supported")}
#'
new_generic <- function(name, fun = NULL, signature = NULL) {
  if (is.null(signature) && is.null(fun)) {
    stop(
      "Must call `new_generic()` with at least one of `signature` or `fun`",
      call. = FALSE
    )
  }

  if (is.null(signature)) {
    signature <- guess_signature(fun)
  } else {
    signature <- check_signature(signature)

    if (is.null(fun)) {
      args <- setNames(lapply(signature, function(i) quote(expr = )), signature)
      fun <- make_function(args, quote(method_call()), topenv(environment()))
    }
  }
  # Never dispatch on ...
  signature <- setdiff(signature, "...")

  R7_generic(name = name, signature = signature, fun = fun)
}

guess_signature <- function(fun) {
  formals <- formals(fun)
  is_required <- vlapply(formals, identical, quote(expr = ))
  names(formals[is_required])
}

check_signature <- function(signature) {
  if (!is.character(signature)) {
    stop("`signature` must be a character vector", call. = FALSE)
  }
  if (length(signature) == 0) {
    stop("`signature` must have at least one component", call. = FALSE)
  }
  signature
}

#' @export
print.R7_generic <- function(x, ...) {
  ms <- methods(x)
  indexes <- seq_along(ms)
  method_signatures <- vcapply(ms, function(x) method_signature(x@signature))

  msg <- collapse(sprintf("%s: method(%s, list(%s))", indexes, x@name, method_signatures), by = "\n")

  formals <- collapse(head(format(args(x)), n = -1), by = "\n")

  cat(sprintf("<R7_generic> %s with %i methods:\n%s", formals, length(ms), msg), sep = "")
}
