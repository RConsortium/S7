# Called from C
method_lookup_error <- function(name, args, signatures) {
  args <- setdiff(args, "...")

  fmt_classes <- function(classes, prefix = NULL) {
    paste0(prefix, "<", classes, ">", collapse = ", ")
  }
  types <- paste0("- ", args, ": ", vcapply(signatures, fmt_classes), collapse = "\n")
  stop(sprintf("Can't find method for generic `%s()` with classes:\n%s", name, types), call. = FALSE)
}

#' @rdname new_generic
#' @order 2
#' @export
method_call <- function() {
  .Call(method_call_, sys.call(-1), sys.function(-1), sys.frame(-1))
}
