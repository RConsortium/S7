#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param class The class to register the method for
#' @param value The new function to use for the method.
#' @export
method <- function(generic, class) {
  if (inherits(class, "r7_class")) {
    class <- class@name
  }
  get_r7_method(generic, class, envir = parent.frame())
}

#' @rdname method
#' @export
method_register <- function(generic, class, value) {
  registerS3method(generic, class, value, envir = parent.frame())
}

get_r7_method <- function(generic, class, envir) {
  # We lookup the registered method in the S3 methods table directly, as R7 generics don't use UseMethod.
  s3_table <- get(".__S3MethodsTable__.", envir)
  get(paste0(generic, ".", class), s3_table)
}
