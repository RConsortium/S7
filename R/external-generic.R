#' Generics in other packages
#'
#' @description
#' To register a method for a generic in another packages you need to
#' `new_external_generic()` and `external_methods_register()` together.
#' `new_external_generic()` defines the "shape" of the generic without
#' requiring the other package be available. You then call
#' `external_methods_register()` in `.onLoad()` to dynamically register
#' the methods when the other package is loaded.
#'
#' @param package Package the generic is defined in.
#' @param name Name of generic, as a string.
#' @param dispatch_args Character vector giving arguments used for dispatch.
#' @param version An optional version the package must meet for the method to
#'   be registered.
#' @export
new_external_generic <- function(package, name, dispatch_args, version = NULL) {
  out <- list(
    package = package,
    name = name,
    dispatch_args = dispatch_args,
    version = version
  )

  class(out) <- "R7_external_generic"
  out
}

#' @export
print.R7_external_generic <- function(x, ...) {
  cat(
    "<R7_external_generic> ",
    x$package, "::", x$name, "(", paste(x$dispatch_args, collapse = ", "), ")",
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
#' @export
external_methods_register <- function() {
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
  # Force all arguments
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
  S3_methods_table(package)[[".R7_methods"]] %||% list()
}

external_methods_reset <- function(package) {
  tbl <- S3_methods_table(package)
  tbl[[".R7_methods"]] <- list()
  invisible()
}

external_methods_add <- function(package, generic, signature, method) {
  tbl <- S3_methods_table(package)

  methods <- append(
    tbl[[".R7_methods"]] %||% list(),
    list(list(generic = generic, signature = signature, method = method))
  )

  tbl[[".R7_methods"]] <- methods
  invisible()
}

S3_methods_table <- function(package) {
  asNamespace(package)[[".__S3MethodsTable__."]]
}
