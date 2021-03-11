#' Define a new generic
#'
#' Use `new_generic` to define a new generic for a package.
#' If you need to reference a generic in another package either import the generic into your
#' package's namespace or use `new_external_generic` to declare it as an
#' external generic and add a `method_register()` call to your `.onLoad`
#' function.
#' @inheritParams R7_generic
#' @export
new_generic <- function(name, signature = NULL, fun = NULL) {
  if (is.null(signature) && is.null(fun)) {
    stop("Must call `new_generic()` with either `signature` or `fun`", call. = FALSE)
  }
  if (is.null(signature)) {
    signature <- formals(fun)[1]
  }
  signature <- normalize_signature(signature)

  if (is.null(fun)) {
    fun <- function() method_call()
    formals(fun) <- signature
    environment(fun) <- topenv(environment())
  }

  R7_generic(name = name, signature, fun = fun)
}

#' @rdname new_generic
#' @param package The package the external generic is defined in.
#' @param version An optional version the package must meet for the method to be registered.
#' @export
new_external_generic <- function(package, generic, signature, version = NULL) {
  out <- list(
    package = package,
    generic = generic,
    signature = signature,
    version = version
  )

  class(out) <- "R7_external_generic"
  out
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
    signature <- out
  }
  signature <- as.list(signature)

  # add ... to the signature if it isn't already there
  if (!("..." %in% names(signature))) {
    signature[["..."]] <- quote(expr = )
  }
  signature
}

#' Generate the body of a generic function
#'
#' This is used as the default to [new_generic]
#' @param name of the generic
#' @param signature signature of the generic
#' @param envir environment to use as the enclosing environment of the generated generic
#' @export
generic_generate <- function(name, signature, envir = parent.frame()) {
  fun <- function() NULL
  formals(fun) <- signature

  sig_call <- generic_generate_signature_call(signature)
  call_args <- names(signature)
  method_call <- as.call(c(as.call(c(as.symbol("method"), as.symbol(name), sig_call)), lapply(call_args, as.symbol)))

  body(fun) <- method_call
  environment(fun) <- envir
  fun
}

generic_generate_signature_call <- function(signature) {
  class_args <- setdiff(names(signature), "...")
  call_args <- names(signature)
  as.call(c(as.symbol("list"), lapply(class_args, function(x) { bquote(object_class(.(arg)), list(arg = as.symbol(x)))})))
}
