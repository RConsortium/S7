#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
#' @param value The new function to use for the method.
#' @export
method <- function(generic, signature) {
  .Call(method_, generic, signature, parent.frame())
}

#' @rdname method
#' @export
method_register <- function(generic, signature, value) {
  registerS3method(generic, paste0(signature, collapse = "-"), value, envir = parent.frame())
}
