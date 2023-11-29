# Called from C
method_lookup_error <- function(name, args) {
  types <- vcapply(args, obj_desc)
  msg <- method_lookup_error_message(name, types)
  cnd <- errorCondition(msg, class = c("methodNotFound", "error"))
  stop(cnd)
}

method_lookup_error_message <- function(name, types) {
  if (length(types) == 1) {
    sprintf("Can't find method for `%s(%s)`.", name, types)
  } else {
    arg_names <- paste0(names(types), collapse = ", ")
    types <- paste0("- ", format(names(types)), ": ", types, collapse = "\n")
    sprintf("Can't find method for generic `%s(%s)`:\n%s", name, arg_names, types)
  }
}

#' @rdname new_generic
#' @order 2
#' @export
S7_dispatch <- function() {
  S7_dispatched_call <- .Call(method_call_, sys.call(-1), sys.function(-1), sys.frame(-1))
  eval(S7_dispatched_call, envir = sys.frame(-1))
}
