#' Define a new generic
#'
#' @inheritParams r7_generic
#' @export
generic_new <- function(name, signature = NULL, fun = NULL) {
  if (is.null(signature) && is.null(fun)) {
    stop("Must call `generic_new()` with either `signature` or `fun`", call. = FALSE)
  }
  if (is.null(signature)) {
    signature <- formals(fun)[1]
  }
  signature <- normalize_signature(signature)

  if (is.null(fun)) {
    fun <- generic_generate(name, signature, envir = parent.frame())
  }

  r7_generic(name = name, signature, fun = fun)
}

normalize_signature <- function(signature, envir = parent.frame()) {
  if (!is_named(signature)) {
    if (!is.character(signature)) {
      stop("`signature` must either be named types or an unnamed character vector of argument names", call. = FALSE)
    }
    out <- vector("list", length(signature))
    for (i in seq_along(out)) {
      out[[i]] <- quote(expr =)
    }
    names(out) <- signature
    return(out)
  }
  as.list(signature)
}

#' Generate the body of a generic function
#'
#' This is used as the default to `generic_new`
#' @param name of the generic
#' @param signature signature of the generic
#' @param envir environment to use as the enclosing environment of the generated generic
#' @export
generic_generate <- function(name, signature, envir = parent.frame()) {
  fun <- function() NULL
  formals(fun) <- signature
  sig_call <- as.call(c(as.symbol("list"), lapply(names(signature), function(x) { bquote(object_class(.(arg)), list(arg = as.symbol(x)))})))
  method_call <- as.call(c(as.call(c(as.symbol("method"), as.symbol(name), sig_call)), lapply(names(signature), as.symbol)))
  body(fun) <- method_call
  environment(fun) <- envir
  fun
}
