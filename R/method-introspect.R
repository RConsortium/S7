#' Retrieve a method for an R7 generic
#'
#' `method()` takes a generic and signature and retrieves the corresponding
#' method. This is rarely needed because most of the time you'll rely on the
#' the generic, via [method_call()], to find and call the method for you.
#' However, this introspection is useful if you want to see the implementation
#' of a specific method.
#'
#' @seealso [method_explain()] to explain why a specific method was picked.
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
  dispatch <- as_dispatch(generic, class = class, object = object)
  .Call(method_, generic, dispatch, NULL)
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
#' * R7: `pkg::class` or `class`
#' * S4: `S4/pkg::class` or `S4/class`
#' * S3: `class`
#'
#' @inheritParams method
#' @export
#' @examples
#' foo1 <- new_class("foo1")
#' foo2 <- new_class("foo2", foo1)
#'
#' add <- new_generic("add", c("x", "y"))
#' method(add, list(foo2, foo1)) <- function(x, y) c(2, 1)
#' method(add, list(foo1, foo1)) <- function(x, y) c(1, 1)
#'
#' method_explain(add, list(foo2, foo2))
method_explain <- function(generic, class = NULL, object = NULL) {
  dispatch <- as_dispatch(generic, class = class, object = object)

  grid <- as.matrix(rev(do.call("expand.grid", rev(dispatch))))
  colnames(grid) <- generic@dispatch_args

  names <- paste0("[", grid, "]")
  dim(names) <- dim(grid)
  methods <- apply(names, 1, paste, collapse = ", ")

  has_method <- function(dispatchs, env) {
    for (x in dispatchs) {
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
  if (!inherits(generic, "R7_generic")) {
    stop("`generic` must be an <R7_generic>")
  }

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

