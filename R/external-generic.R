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
#' @param generic Name of generic, as a string.
#' @param version An optional version the package must meet for the method to
#'   be registered.
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

#' @importFrom utils getFromNamespace packageName
#' @rdname new_external_generic
#' @export
method_register <- function() {
  package <- packageName(parent.frame())
  tbl <- asNamespace(package)[[".__S3MethodsTable__."]][[".R7_methods"]]
  for (x in tbl) {
    if (isNamespaceLoaded(x$package)) {
      ns <- asNamespace(x$package)
      new_method(getFromNamespace(x$generic, ns), x$signature, x$method)
    } else {
      setHook(packageEvent(x$package, "onLoad"),
        local({
          x <- x
          function(...) {
            ns <- asNamespace(x$package)
            if (is.null(x$version) || getNamespaceVersion(ns) >= x$version) {
              new_method(getFromNamespace(x$generic, ns), x$signature, x$method)
            }
          }
        })
      )
    }
  }
}
