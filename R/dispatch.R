#' Retrieve a method for an R7 generic
#'
#' `method()` takes a generic and signature and retrieves the correspoding
#' method. This is rarely needed because most of the time you'll rely on the
#' the generic, via [method_call()], to find and call the method for you.
#' However, this introspection is useful if you want to see the implementation
#' of a specific method.
#'
#' @inheritParams method<-
#' @returns A function with class <R7_method>.
#' @export
#' @examples
#' # Create a generic and register some methods
#' bizarro <- new_generic("bizarro", dispatch_args = "x")
#' method(bizarro, "numeric") <- function(x) rev(x)
#' method(bizarro, s3_class("factor")) <- function(x) {
#'   levels(x) <- rev(levels(x))
#'   x
#' }
#'
#' # Printing the generic shows the registered method
#' bizarro
#'
#' # And you can use method() to inspect specific implementations
#' method(bizarro, "integer")
#' method(bizarro, s3_class("factor"))
method <- function(generic, signature) {
  if (!inherits(generic, "R7_generic")) {
    stop("`generic` must be an <R7_generic>")
  }

  signature <- as_signature(signature)
  is_union <- vlapply(signature, is_union)
  if (any(is_union)) {
    stop("Can't dispatch on unions; must be a concrete type")
  }

  .Call(method_, generic, signature, NULL)
}

# Called from C
method_lookup_error <- function(name, args, signatures) {
  args <- setdiff(args, "...")
  types <- paste0("- ", args, ": ", vcapply(signatures, fmt_classes), collapse = "\n")
  stop(sprintf("Can't find method for generic `%s()` with classes:\n%s", name, types), call. = FALSE)
}

#' @rdname new_generic
#' @order 2
#' @export
method_call <- function() {
  .Call(method_call_, sys.call(-1), sys.function(-1), sys.frame(-1))
}
