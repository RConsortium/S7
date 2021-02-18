#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param signature The method signature
#' @param value The new function to use for the method.
#' @export
method <- function(generic, signature) {
  if (!is.list(signature)) {
    signature <- list(signature)
  }
  for (i in seq_along(signature)) {
    if (inherits(signature[[i]], "r7_class")) {
      signature[[i]] <- class_names(signature[[i]])
    }
  }
  signatures <- do.call(paste, c(sep = "-", do.call(expand.grid, signature)))
  for (s in signatures) {
    fun <- get_r7_method(generic, s, envir = parent.frame())
    if (!is.null(fun)) {
      return(fun)
    }
  }
  stop(sprintf("No methods found for generic '%s' for classes:\n%s", generic, paste0("- ", signature,  collapse = "\n"), call. = FALSE))
}

get_r7_method <- function(generic, class, envir) {
  # We lookup the registered method in the S3 methods table directly, as R7 generics don't use UseMethod.
  s3_table <- get(".__S3MethodsTable__.", envir)
  get0(paste0(generic, ".", class), s3_table)
}

#' @rdname method
#' @export
method_register <- function(generic, signature, value) {
  registerS3method(generic, paste0(signature, collapse = "-"), value, envir = parent.frame())
}
