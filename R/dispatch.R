#' Retrieve a method for an R7 generic
#'
#' `method()` takes a generic and signature and retrieves the corresponding
#' method. This is rarely needed because most of the time you'll rely on the
#' the generic, via [method_call()], to find and call the method for you.
#' However, this introspection is useful if you want to see the implementation
#' of a specific method.
#'
#' @inheritParams method<-
#' @returns A function with class <R7_method>.
#' @param class,object Perform introspection either with `classes`
#'   (processed with [as_class()]) or a concrete objects.
#'
#'   If `generic` does multiple dispatch both `object` and `class` need
#'   to be wrapped in a list.
#' @export
#' @examples
#' # Create a generic and register some methods
#' bizarro <- new_generic("bizarro", "x")
#' method(bizarro, "numeric") <- function(x) rev(x)
#' method(bizarro, new_S3_class("factor")) <- function(x) {
#'   levels(x) <- rev(levels(x))
#'   x
#' }
#'
#' # Printing the generic shows the registered method
#' bizarro
#'
#' # And you can use method() to inspect specific implementations
#' method(bizarro, class = "integer")
#' method(bizarro, object = 1)
#' method(bizarro, new_S3_class("factor"))
method <- function(generic, class = NULL, object = NULL) {
  if (!inherits(generic, "R7_generic")) {
    stop("`generic` must be an <R7_generic>")
  }
  if (!xor(is.null(class), is.null(object))) {
    stop("Must supply exactly one of `class` and `object`")
  }

  if (!is.null(class)) {
    signature <- as_signature(class, generic)
    is_union <- vlapply(signature, is_union)
    if (any(is_union)) {
      stop("Can't dispatch on unions; must be a concrete type")
    }

    dispatch <- lapply(signature, class_dispatch)
  } else {
    n <- generic_n_dispatch(generic)
    if (n == 1) {
      object <- list(object)
    } else {
      check_signature_list(object, n = n, arg = "object")
    }
    dispatch <- lapply(object, obj_dispatch)
  }

  .Call(method_, generic, dispatch, NULL)
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
