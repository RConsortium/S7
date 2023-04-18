# Called from C
method_lookup_error <- function(name, args, signatures) {
  sigs <- vcapply(signatures, paste, collapse = ", ")
  types <- vcapply(args, obj_desc)

  if (length(args) == 1) {
    msg <- sprintf("Can't find method for `%s(%s)`.", name, types)
  } else {
    arg_names <- paste0(names(args), collapse = ", ")
    types <- paste0("- ", format(names(args)), ": ", types, collapse = "\n")
    msg <- sprintf("Can't find method for generic `%s(%s)`:\n%s", name, arg_names, types)
  }

  stop(msg, call. = FALSE)
}

#' @rdname new_generic
#' @order 2
#' @export
S7_dispatch <- function() {
  S7_dispatched_call <- .Call(method_call_, sys.call(-1), sys.function(-1), sys.frame(-1))
  eval(S7_dispatched_call, envir = sys.frame(-1))
}
