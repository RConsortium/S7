#' Define a new generic
#'
#' @description
#' Use `new_generic()` to create a new generic, then [method<-] to add methods
#' to it.
#'
#' See [new_external_generic()] if you need to define a method for a generic
#' in another package without taking a strong dependency on it.
#'
#'
#' @inheritParams R7_generic
#' @export
#' @examples
new_generic <- function(name, signature = NULL, fun = NULL) {
  if (is.null(signature) && is.null(fun)) {
    stop("Must call `new_generic()` with either `signature` or `fun`", call. = FALSE)
  }
  if (is.null(signature)) {
    signature <- guess_signature(fun)
  } else {
    signature <- normalize_signature(signature)
  }

  if (is.null(fun)) {
    fun <- function() method_call()
    formals(fun) <- signature
    environment(fun) <- topenv(environment())
  }

  R7_generic(name = name, signature, fun = fun)
}

#' Define an external generic
#'
#' If you need to reference a generic in another package either import the generic into your
#' package's namespace or use `new_external_generic` to declare it as an
#' external generic and add a `method_register()` call to your `.onLoad`
#' function.
#' @rdname new_generic
#' @param package The package the external generic is defined in.
#' @param version An optional version the package must meet for the method to be registered.
#' @export
new_external_generic <- function(package, generic, version = NULL) {
  out <- list(
    package = package,
    generic = generic,
    version = version
  )

  class(out) <- "R7_external_generic"
  out
}


guess_signature <- function(fun) {
  formals <- formals(fun)
  is_required <- vlapply(formals, identical, quote(expr = ))
  setdiff(names(formals[is_required]), "...")
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

generic_generate_signature_call <- function(signature) {
  class_args <- setdiff(names(signature), "...")
  call_args <- names(signature)
  as.call(c(as.symbol("list"), lapply(class_args, function(x) { bquote(object_class(.(arg)), list(arg = as.symbol(x)))})))
}

#' @export
print.R7_generic <- function(x, ...) {
  ms <- methods(x)
  indexes <- seq_along(ms)
  method_signatures <- vcapply(ms, function(x) method_signature(x@signature))

  msg <- collapse(sprintf("%s: method(%s, list(%s))", indexes, x@name, method_signatures), by = "\n")

  formals <- collapse(head(format(args(x)), n = -1), by = "\n")

  cat(sprintf("<R7_generic> %s with %i methods:\n%s", formals, length(ms), msg), sep = "")
}
