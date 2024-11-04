#' Define a new generic
#'
#' @description
#' A generic function uses different implementations (_methods_) depending on
#' the class of one or more arguments (the _signature_). Create a new generic
#' with `new_generic()` then use [method<-] to add methods to it.
#'
#' Method dispatch is performed by `S7_dispatch()`, which must always be
#' included in the body of the generic, but in most cases `new_generic()` will
#' generate this for you.
#'
#' Learn more in `vignette("generics-methods")`
#'
#' @section Dispatch arguments:
#' The arguments that are used to pick the method are called the **dispatch
#' arguments**. In most cases, this will be one argument, in which case the
#' generic is said to use **single dispatch**. If it consists of more than
#' one argument, it's said to use **multiple dispatch**.
#'
#' There are two restrictions on the dispatch arguments: they must be the first
#' arguments to the generic and if the generic uses `...`, it must occur
#' immediately after the dispatch arguments.
#'
#' @param name The name of the generic. This should be the same as the object
#'   that you assign it to.
#' @param dispatch_args A character vector giving the names of one or more
#'   arguments used to find the method.
#' @param fun An optional specification of the generic, which must call
#'  `S7_dispatch()` to dispatch to methods. This is usually generated
#'  automatically from the `dispatch_args`, but you may want to supply it if
#'  you want to add additional required arguments, omit `...`, or perform
#'  some standardised computation in the generic.
#'
#'  The `dispatch_args` must be the first arguments to `fun`, and, if present,
#'  `...` must immediately follow them.
#' @seealso [new_external_generic()] to define a method for a generic
#'  in another package without taking a strong dependency on it.
#' @export
#' @returns An S7 generic, i.e. a function with class `S7_generic`.
#' @order 1
#' @examples
#' # A simple generic with methods for some base types and S3 classes
#' type_of <- new_generic("type_of", dispatch_args = "x")
#' method(type_of, class_character) <- function(x, ...) "A character vector"
#' method(type_of, new_S3_class("data.frame")) <- function(x, ...) "A data frame"
#' method(type_of, class_function) <- function(x, ...) "A function"
#'
#' type_of(mtcars)
#' type_of(letters)
#' type_of(mean)
#'
#' # If you want to require that methods implement additional arguments,
#' # you can use a custom function:
#' mean2 <- new_generic("mean2", "x", function(x, ..., na.rm = FALSE) {
#'    S7_dispatch()
#' })
#'
#' method(mean2, class_numeric) <- function(x, ..., na.rm = FALSE) {
#'   if (na.rm) {
#'     x <- x[!is.na(x)]
#'   }
#'   sum(x) / length(x)
#' }
#'
#' # You'll be warned if you forget the argument:
#' method(mean2, class_character) <- function(x, ...) {
#'   stop("Not supported")
#' }
new_generic <- function(name, dispatch_args, fun = NULL) {
  check_name(name)

  dispatch_args <- check_dispatch_args(dispatch_args, fun)

  if (is.null(fun)) {
    args <- c(dispatch_args, "...")
    args <- setNames(lapply(args, function(i) quote(expr = )), args)
    fun <- new_function(args, quote(S7::S7_dispatch()), parent.frame())
  } else {
    check_generic(fun)
  }

  S7_generic(fun, name = name, dispatch_args = dispatch_args)
}

check_dispatch_args <- function(dispatch_args, fun = NULL) {
  if (!is.character(dispatch_args)) {
    stop("`dispatch_args` must be a character vector", call. = FALSE)
  }
  if (length(dispatch_args) == 0) {
    stop("`dispatch_args` must have at least one component", call. = FALSE)
  }
  if (anyDuplicated(dispatch_args)) {
    stop("`dispatch_args` must be unique", call. = FALSE)
  }
  if (any(is.na(dispatch_args) | dispatch_args == "")) {
    stop("`dispatch_args` must not be missing or the empty string")
  }
  if ("..." %in% dispatch_args) {
    stop("Can't dispatch on `...`", call. = FALSE)
  }

  if (!is.null(fun)) {
    arg_names <- names(formals(fun))

    if (!is_prefix(dispatch_args, arg_names)) {
      stop("`dispatch_args` must be a prefix of the generic arguments", call. = FALSE)
    }
  }

  dispatch_args
}

#' @export
print.S7_generic <- function(x, ...) {
  methods <- methods(x)
  formals <- show_args(formals(x), x@name)
  cat(sprintf("<S7_generic> %s with %i methods:\n", formals, length(methods)), sep = "")

  if (length(methods) > 0) {
    signatures <- lapply(methods, prop, "signature")
    msg <- vcapply(signatures, method_signature, generic = x)
    msg <- paste0(format(seq_along(signatures)), ": ", msg, "\n")
    cat(msg, sep = "")
  }

  invisible(x)
}

check_generic <- function(fun) {
  if (!is.function(fun)) {
    stop("`fun` must be a function", call. = FALSE)
  }

  dispatch_call <- find_call(body(fun), quote(S7_dispatch), packageName())
  if (is.null(dispatch_call)) {
    stop("`fun` must contain a call to `S7_dispatch()`", call. = FALSE)
  }
}

#' Recursively find a call (namespaced or plain)
#'
#' @param x An language object
#' @param name A name/symbol
#' @param ns A string. If `NULL` (the default), only unnamespaced calls are
#'   matched.  If a string, the call may also match a `ns`-qualified call.
#' @return `call` object if found; `NULL` otherwise.
#' @noRd
find_call <- function(x, name, ns = NULL) {
  if (!is.call(x)) {
    return(NULL)
  }

  # is namespaced `ns::name(...)` or plain `name(...)` call
  if (is_ns_call(x[[1]], name, ns) || identical(x[[1]], name)) {
    return(x)
  }

  # otherwise, recurse through arguments
  if (length(x) > 1) {
    for (i in seq(2, length(x))) {
      call <- find_call(x[[i]], name = name, ns = ns)
      if (!is.null(call)) {
        return(call)
      }
    }
  }
  NULL
}

is_ns_call <- function(x, name, ns = NULL) {
  if (is.null(ns)) return(FALSE)

  length(x) == 3 &&
    identical(x[[2]], as.symbol(ns)) &&
    identical(x[[1]], quote(`::`)) &&
    identical(x[[3]], name)
}

methods <- function(generic) {
  methods_rec(generic@methods, character())
}
methods_rec <- function(x, signature) {
  if (!is.environment(x)) {
    return(x)
  }

  # Recursively collapse environments to a list
  methods <- lapply(names(x), function(class) methods_rec(x[[class]], c(signature, class)))
  unlist(methods, recursive = FALSE)
}

generic_add_method <- function(generic, signature, method) {
  p_tbl <- generic@methods
  chr_signature <- vcapply(signature, class_register)

  if (is.null(attr(method, "name", TRUE)))
    attr(method, "name") <- as.name(method_signature(generic, signature))

  for (i in seq_along(chr_signature)) {
    class_name <- chr_signature[[i]]
    if (i != length(chr_signature)) {
      # Iterated dispatch, so create another nested environment
      tbl <- p_tbl[[class_name]]
      if (is.null(tbl)) {
        tbl <- new.env(hash = TRUE, parent = emptyenv())
        p_tbl[[class_name]] <- tbl
      }
      p_tbl <- tbl
    } else {
      if (!is.null(p_tbl[[class_name]])) {
        message("Overwriting method ", method_name(generic, signature))
      }
      p_tbl[[class_name]] <- method
    }
  }
}
