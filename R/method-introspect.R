#' Find a method for an S7 generic
#'
#' `method()` takes a generic and class signature and performs method dispatch
#' to find the corresponding method implementation. This is rarely needed
#' because you'll usually rely on the the generic to do dispatch for you (via
#' [S7_dispatch()]). However, this introspection is useful if you want to see
#' the implementation of a specific method.
#'
#' @seealso [method_explain()] to explain why a specific method was picked.
#' @inheritParams method<-
#' @returns Either a function with class `S7_method` or an error if no
#'   matching method is found.
#' @param class,object Perform introspection either with a `class`
#'   (processed with [as_class()]) or a concrete `object`. If `generic` uses
#'   multiple dispatch then both `object` and `class` must be a list of
#'   classes/objects.
#' @export
#' @examples
#' # Create a generic and register some methods
#' bizarro <- new_generic("bizarro", "x")
#' method(bizarro, class_numeric) <- function(x) rev(x)
#' method(bizarro, class_factor) <- function(x) {
#'   levels(x) <- rev(levels(x))
#'   x
#' }
#'
#' # Printing the generic shows the registered method
#' bizarro
#'
#' # And you can use method() to inspect specific implementations
#' method(bizarro, class = class_integer)
#' method(bizarro, object = 1)
#' method(bizarro, class = class_factor)
#'
#' # errors if method not found
#' try(method(bizarro, class = class_data.frame))
#' try(method(bizarro, object = "x"))
method <- function(generic, class = NULL, object = NULL) {
  check_is_S7(generic, S7_generic)
  dispatch <- as_dispatch(generic, class = class, object = object)

  method <- .Call(method_, generic, dispatch, environment(), FALSE)
  if (!is.null(method)) {
    return(method)
  }

  # can't rely on usual error mechanism because it involves looking up
  # argument values in the dispatch environment, which doesn't exist here
  types <- error_types(generic, class = class, object = object)
  msg <- method_lookup_error_message(generic@name, types)
  stop(msg, call. = FALSE)
}

#' Explain method dispatch
#'
#' @description
#' `method_explain()` shows all possible methods that a call to a generic
#' might use, which ones exist, and which one will actually be called.
#'
#' Note that method dispatch uses a string representation of each class in
#' the class hierarchy. Each class system uses a slightly different convention
#' to avoid ambiguity.
#'
#' * S7: `pkg::class` or `class`
#' * S4: `S4/pkg::class` or `S4/class`
#' * S3: `class`
#'
#' @inheritParams method
#' @return Nothing; this function is called for it's side effects.
#' @export
#' @examples
#' Foo1 <- new_class("Foo1")
#' Foo2 <- new_class("Foo2", Foo1)
#'
#' add <- new_generic("add", c("x", "y"))
#' method(add, list(Foo2, Foo1)) <- function(x, y) c(2, 1)
#' method(add, list(Foo1, Foo1)) <- function(x, y) c(1, 1)
#'
#' method_explain(add, list(Foo2, Foo2))
method_explain <- function(generic, class = NULL, object = NULL) {
  check_is_S7(generic, S7_generic)
  dispatch <- as_dispatch(generic, class = class, object = object)
  dispatch <- lapply(dispatch, c, "ANY")

  grid <- as.matrix(rev(do.call("expand.grid", rev(dispatch))))
  colnames(grid) <- generic@dispatch_args

  names <- paste0("[", grid, "]")
  dim(names) <- dim(grid)
  methods <- apply(names, 1, paste, collapse = ", ")

  has_method <- function(dispatches, env) {
    for (x in dispatches) {
      env <- env[[x]]
    }
    is.function(env)
  }
  exists <- apply(grid, 1, has_method, env = generic@methods)

  label <- ifelse(exists, "* ", "  ")
  if (any(exists)) {
    label[which(exists)[[1]]] <- "->"
  }

  cat(paste0(label, " ", generic@name, "(", methods, ")\n"), sep = "")
  invisible()
}


as_dispatch <- function(generic, class = NULL, object = NULL) {
  if (!is.null(class) && is.null(object)) {
    signature <- as_signature(class, generic)
    is_union <- vlapply(signature, is_union)
    if (any(is_union)) {
      stop("Can't dispatch on unions; must be a concrete type")
    }

    lapply(signature, class_dispatch)
  } else if (!is.null(object) && is.null(class)) {
    n <- generic_n_dispatch(generic)
    if (n == 1) {
      object <- list(object)
    } else {
      check_signature_list(object, n = n, arg = "object")
    }
    lapply(object, obj_dispatch)
  } else {
    stop("Must supply exactly one of `class` and `object`", call. = FALSE)
  }
}

error_types <- function(generic, class = NULL, object = NULL) {
  if (is.null(class)) {
    n <- generic_n_dispatch(generic)
    if (n == 1) {
      types <- list(obj_desc(object))
    } else {
      types <- vcapply(object, obj_desc)
    }
  } else {
    signature <- as_signature(class, generic)
    types <- vcapply(signature, class_desc)
  }
  names(types) <- generic@dispatch_args
  types
}
