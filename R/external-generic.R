#' Generics in other packages
#'
#' @description
#' You need an explicit external generic when you want to provide methods
#' for a generic (S3, S4, or S7) that is defined in another package, and you
#' don't want to take a hard dependency on that package.
#'
#' The easiest way to provide methods for generics in other packages is
#' import the generic into your `NAMESPACE`. This, however, creates a hard
#' dependency, and sometimes you want a soft dependency, only registering the
#' method if the package is already installed. `new_external_generic()` allows
#' you to provide the minimal needed information about a generic so that methods
#' can be registered at run time, as needed, using [methods_register()].
#'
#' Note that in tests, you'll need to explicitly call the generic from the
#' external package with `pkg::generic()`.
#'
#' @param package Package the generic is defined in.
#' @param name Name of generic, as a string.
#' @param dispatch_args Character vector giving arguments used for dispatch.
#' @param version An optional version the package must meet for the method to
#'   be registered.
#' @returns An S7 external generic, i.e. a list with class
#'   `S7_external_generic`.
#' @export
#' @examples
#' my_class <- new_class("my_class")
#'
#' your_generic <- new_external_generic("stats", "median", "x")
#' method(your_generic, my_class) <- function(x) "Hi!"
new_external_generic <- function(package, name, dispatch_args, version = NULL) {
  out <- list(
    package = package,
    name = name,
    dispatch_args = dispatch_args,
    version = version
  )

  class(out) <- "S7_external_generic"
  out
}

#' @export
print.S7_external_generic <- function(x, ...) {
  cat(
    "<S7_external_generic> ",
    x$package, "::", x$name, "(", paste(x$dispatch_args, collapse = ", "), ")",
    if (!is.null(x$version)) paste0(" (>= ", x$version, ")"),
    "\n",
    sep = ""
  )
  invisible(x)
}

is_external_generic <- function(x) {
  inherits(x, "S7_external_generic")
}

#' Register methods in a package
#'
#' When using S7 in a package you should always call `methods_register()` when
#' your package is loaded. This ensures that methods are registered as needed
#' when you implement methods for generics (S3, S4, and S7) in other packages.
#' (This is not strictly necessary if you only register methods for generics
#' in your package, but it's better to include it and not need it than forget
#' to include it and hit weird errors.)
#'
#' @importFrom utils getFromNamespace packageName
#' @export
#' @returns Nothing; called for its side-effects.
#' @examples
#' .onLoad <- function(...) {
#'   S7::methods_register()
#' }
methods_register <- function() {
  package <- packageName(parent.frame())
  tbl <- external_methods_get(package)

  for (x in tbl) {
    register <- registrar(x$generic, x$signature, x$method)

    if (isNamespaceLoaded(x$generic$package)) {
      register()
    } else {
      setHook(packageEvent(x$generic$package, "onLoad"), register)
    }
  }

  invisible()
}

registrar <- function(generic, signature, method) {
  # Force all arguments
  list(generic, signature, method)

  function(...) {
    ns <- asNamespace(generic$package)
    if (is.null(generic$version) || getNamespaceVersion(ns) >= generic$version) {
      if (!exists(generic$name, envir = ns, inherits = FALSE)) {
        msg <- sprintf("[S7] Failed to find generic %s() in package %s", generic$name, generic$package)
        warning(msg, call. = FALSE)
      } else {
        generic_fun <- get(generic$name, envir = ns, inherits = FALSE)
        register_method(generic_fun, signature, method, package = NULL)
      }
    }
  }
}

external_methods_get <- function(package) {
  S3_methods_table(package)[[".S7_methods"]] %||% list()
}

external_methods_reset <- function(package) {
  tbl <- S3_methods_table(package)
  tbl[[".S7_methods"]] <- list()
  invisible()
}

external_methods_add <- function(package, generic, signature, method) {
  tbl <- S3_methods_table(package)

  methods <- append(
    tbl[[".S7_methods"]] %||% list(),
    list(list(generic = generic, signature = signature, method = method))
  )

  tbl[[".S7_methods"]] <- methods
  invisible()
}

S3_methods_table <- function(package) {
  asNamespace(package)[[".__S3MethodsTable__."]]
}
