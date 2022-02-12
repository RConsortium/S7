#' Define a new generic
#'
#' @description
#' A generic function uses different implementations (_methods_) depending on
#' the class of one or more arguments (the _signature_). Create a new generic
#' with `new_generic()` then use [method<-] to add methods to it. The body of
#' the generic always contains `method_call()`, which takes care of finding and
#' calling the appropriate method.
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
#' @param dispatch_args A character vector giving the names of the arguments
#'   that form the signature, i.e. the arguments used for method dispatch.
#'
#'   If `dispatch_args` are omitted, but `fun` is supplied, will default to the
#'   arguments that appear before `...` in `fun`. If there are no dots, it will
#'   default to the first argument. If both `fun` and `dispatch_args` are
#'   supplied, the `dispatch_args` must appear at the start of `fun`'s formals.
#'
#' @param fun An optional specification of the generic, which must call
#'  `method_call()` to dispatch to methods. This is usually generated
#'  automatically from the `dispatch_args`, but you may want to supply it if
#'  you want to add additional required arguments, or perform some standardised
#'  computation in the generic.
#' @seealso [new_external_generic()] to define a method for a generic
#'  in another package without taking a strong dependency on it.
#' @export
#' @order 1
#' @examples
#' # A simple generic with methods for some base types and S3 classes
#' type_of <- new_generic("type_of", dispatch_args = "x")
#' method(type_of, "character") <- function(x, ...) "A character vector"
#' method(type_of, s3_class("data.frame")) <- function(x, ...) "A data frame"
#' method(type_of, "function") <- function(x, ...) "A function"
#'
#' type_of(mtcars)
#' type_of(letters)
#' type_of(mean)
#'
#' # If you want to require methods implement additional arguments, supply
#' # them after ... in the call
#' mean2 <- new_generic("mean2", fun = function(x, ..., na.rm = TRUE) {
#'    method_call()
#' })
#' method(mean2, "numeric") <- function(x, ..., na.rm = TRUE) {
#'   if (na.rm) {
#'     x <- x[!is.na(x)]
#'   }
#'   sum(x) / length(x)
#' }
#' method(mean2, "character") <- function(x, ..., na.rm = TRUE) {
#'   stop("Not supported")
#' }
#'
new_generic <- function(name, dispatch_args = NULL, fun = NULL) {
  check_name(name)

  if (is.null(dispatch_args) && is.null(fun)) {
    stop(
      "Must call `new_generic()` with at least one of `dispatch_args` or `fun`",
      call. = FALSE
    )
  }

  if (is.null(dispatch_args)) {
    check_generic(fun)
    dispatch_args <- guess_dispatch_args(fun)
  } else {
    dispatch_args <- check_dispatch_args(dispatch_args, fun)

    if (is.null(fun)) {
      args <- c(dispatch_args, "...")
      args <- setNames(lapply(args, function(i) quote(expr = )), args)
      fun <- make_function(args, quote(method_call()), topenv(environment()))
    }
  }

  R7_generic(name = name, dispatch_args = dispatch_args, fun = fun)
}

guess_dispatch_args <- function(fun) {
  formals <- formals(fun)
  # all arguments before ...
  if (length(formals) == 0) {
    character()
  } else if ("..." %in% names(formals)) {
    names(formals)[seq_len(which(names(formals) == "...") - 1)]
  } else {
    names(formals)[[1]]
  }
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

    if ("..." %in% arg_names && arg_names[[length(dispatch_args) + 1]] != "...") {
      stop("If present, ... must immediately follow the `dispatch_args`", call. = FALSE)
    }
  }

  dispatch_args
}

#' @export
print.R7_generic <- function(x, ...) {
  methods <- methods(x)
  formals <- collapse(head(format(args(x)), n = -1), by = "\n")
  cat(sprintf("<R7_generic> %s with %i methods:\n", formals, length(methods)), sep = "")

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

  method_call <- find_call(body(fun), quote(method_call))
  if (is.null(method_call)) {
    stop("`fun` must contain a call to `method_call()`", call. = FALSE)
  }
}
find_call <- function(x, name) {
  if (is.call(x)) {
    if (identical(x[[1]], name)) {
      return(x)
    } else if (length(x) > 1) {
      for (i in seq(2, length(x))) {
        call <- find_call(x[[i]], name)
        if (!is.null(call)) {
          return(call)
        }
      }
    }
  }
  NULL
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
  chr_signature <- vcapply(signature, r7_class_name)

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
      p_tbl[[class_name]] <- method
    }
  }
}
