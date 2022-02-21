# Called from C
method_lookup_error <- function(name, args, signatures) {
  sigs <- vcapply(signatures, paste, collapse = ", ")
  types <- paste0("- ", format(args), ": ", sigs, collapse = "\n")
  msg <- sprintf(
    "Can't find method for generic `%s()` with dispatch classes:\n%s",
    name,
    types
  )
  stop(msg, call. = FALSE)
}

#' @rdname new_generic
#' @order 2
#' @export
method_call <- function() {
  .Call(method_call_, sys.call(-1), sys.function(-1), sys.frame(-1))
}
