#' Retrieve or register an r7 method for a generic
#'
#' @param generic The generic to retrieve or register
#' @param class,classes The class to register the method for
#' @param value The new function to use for the method.
#' @export
method <- function(generic, classes) {
  if (inherits(classes, "r7_class")) {
    classes <- class_names(classes)
  }

  i <- 1
  fun <- get_r7_method(generic, classes[[i]], envir = parent.frame())
  while(is.null(fun) && i < length(classes)) {
    i <- i + 1
    fun <- get_r7_method(generic, classes[[i]], envir = parent.frame())
  }
  if (is.null(fun)) {
    stop(sprintf("No methods defined for generic '%s' with classes:\n%s", generic, paste0("- ", classes,  collapse = "\n"), call. = FALSE))
  }
  fun
}

get_r7_method <- function(generic, class, envir) {
  # We lookup the registered method in the S3 methods table directly, as R7 generics don't use UseMethod.
  s3_table <- get(".__S3MethodsTable__.", envir)
  get0(paste0(generic, ".", class), s3_table)
}

#' @rdname method
#' @export
method_register <- function(generic, class, value) {
  registerS3method(generic, class, value, envir = parent.frame())
}
