# Called from C
dispatch_not_generic_error <- function() {
  stop2(
    "Must be called from within an S7 generic.",
    call = quote(S7_dispatch())
  )
}

# Called from C
method_lookup_error <- function(name, args) {
  types <- vcapply(args, obj_desc)
  msg <- method_lookup_error_message(name, types)
  stop2(msg, call = NULL, class = "S7_error_method_not_found")
}

method_lookup_error_message <- function(name, types) {
  if (length(types) == 1) {
    sprintf("Can't find method for `%s(%s)`.", name, types)
  } else {
    arg_names <- paste0(names(types), collapse = ", ")
    types <- paste0("- ", format(names(types)), ": ", types, collapse = "\n")
    sprintf(
      "Can't find method for generic `%s(%s)`:\n%s",
      name,
      arg_names,
      types
    )
  }
}

#' @rdname new_generic
#' @order 2
#' @export
S7_dispatch <- function() {
  .External2(method_call_, sys.function(-1L), sys.frame(-1L))
}
