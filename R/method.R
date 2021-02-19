#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
#' @param value The new function to use for the method.
#' @export
method <- function(generic, signature) {
  #if (!is.list(signature)) {
    #signature <- list(signature)
  #}
  for (i in seq_along(signature)) {
    #if (inherits(signature[[i]], "r7_class")) {
      signature[[i]] <- .Call(class_names_, signature[[i]])
    #}
  }
  signatures <- .Call(construct_signature_, signature)
  .Call(get_r7_method_, generic, signatures, parent.frame())
}

#' @rdname method
#' @export
method_register <- function(generic, signature, value) {
  registerS3method(generic, paste0(signature, collapse = "-"), value, envir = parent.frame())
}
