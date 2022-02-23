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

#' Retrieve the next applicable method after the current one
#'
#' @export
#' @keywords internal
next_method <- function() {
  current_method <- sys.function(sys.parent(1))

  # Travel up the call stack, finding all methods that have already been called
  methods <- list()
  i <- 1
  while (!inherits(current_method, "R7_generic")) {
    methods <- c(methods, current_method)
    i <- i + 1
    current_method <- sys.function(sys.parent(i))
  }

  generic <- current_method

  # Find signature
  vals <- mget(generic@dispatch_args, envir = parent.frame())
  dispatch <- lapply(vals, obj_dispatch)
  .Call(method_, generic, dispatch, ignore = methods)
}
