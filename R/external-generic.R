#' Generics in suggested packages
#'
#' @description
#' The easiest way to define a method for a generic in another package is to
#' add the package to `Imports` and import the generic into the `NAMESPACE`.
#' This, however, creates a strong dependency on the other package, which is
#' not always desired. For example, you might want to register a
#' `knitr::knitr_print` method to customise how your object is printed in Rmd,
#' but your package doesn't use anything else from knitr.
#'
#' Instead, you can add the package to `Suggests` and use
#' `new_external_generic()` along with `method_register()` to declare an
#' "external" generic. `new_external_generic()` defines the "shape" of the
#' generic without requiring the other package be available. You then call
#' `method_register()` in `.onLoad()` to dynamically register the methods
#' when the other package is loaded.
#'
#' @param package Package the generic is defined in.
#' @param name Name of generic, as a string.
#' @param version An optional version the package must meet for the method to
#'   be registered.
#' @export
new_external_generic <- function(package, name, version = NULL) {
  out <- list(
    package = package,
    name = name,
    version = version
  )

  class(out) <- "R7_external_generic"
  out
}

#' @export
print.R7_external_generic <- function(x, ...) {
  cat(
    "<R7_external_generic> ",
    x$package, "::", x$name, "()",
    if (!is.null(x$version)) paste0(" (>= ", x$version, ")"),
    "\n",
    sep = ""
  )
  invisible(x)
}

is_external_generic <- function(x) {
  inherits(x, "R7_external_generic")
}

#' @importFrom utils getFromNamespace packageName
#' @rdname new_external_generic
#' @param package Package name. Advanced use only.
#' @export
method_register <- function() {
  package <- packageName(parent.frame())
  tbl <- external_methods_get(package)

  for (x in tbl) {
    hook <- registrar(x$generic, x$signature, x$method)

    if (isNamespaceLoaded(x$generic$package)) {
      hook()
    } else {
      setHook(packageEvent(x$generic$package, "onLoad"), hook)
    }
  }
}

registrar <- function(generic, signature, method) {
  list(generic, signature, method)

  function(...) {
    ns <- asNamespace(generic$package)
    if (is.null(generic$version) || getNamespaceVersion(ns) >= generic$version) {
      generic_fun <- getFromNamespace(generic$name, ns)
      register_method(generic_fun, signature, method)
    }
  }
}

external_methods_get <- function(package) {
  s3_methods_table(package)[[".R7_methods"]] %||% list()
}

external_methods_reset <- function(package) {
  tbl <- s3_methods_table(package)
  tbl[[".R7_methods"]] <- list()
  invisible()
}

external_methods_add <- function(package, generic, signature, method) {
  tbl <- s3_methods_table(package)

  methods <- append(
    tbl[[".R7_methods"]] %||% list(),
    list(list(generic = generic, signature = signature, method = method))
  )

  tbl[[".R7_methods"]] <- methods
  invisible()
}

s3_methods_table <- function(package) {
  asNamespace(package)[[".__S3MethodsTable__."]]
}
