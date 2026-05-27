#' Classes in other packages
#'
#' @description
#' An external class is a lightweight placeholder for an S7 class defined in
#' another package (or in your own package, but not yet defined). It carries
#' only the package and class name, and is resolved to the real S7 class
#' when needed.
#'
#' External classes are useful in two situations:
#'
#' * To register a method for a generic in your package, dispatching on a class
#'   from a soft dependency: `method(my_generic, new_external_class("pkg",
#'   "SomeClass")) <- ...`. The method will be registered when `pkg` is loaded
#'   (using the same machinery as [new_external_generic()]).
#'
#' * To refer to a class that hasn't been defined yet, such as a self-referential
#'   or mutually recursive class. For example, you can create a nested class
#'   with `new_class("tree", properties = list(child = NULL |
#'   new_external_class("mypkg", "tree")))`.
#'
#' Make sure to call [methods_register()] in your package's `.onLoad()` so that
#' deferred method registrations fire when the relevant package is loaded.
#'
#' @param package Package the class is defined in.
#' @param name Name of the class, as a string.
#' @param version An optional version that `package` must meet for any
#'   deferred method registration to fire.
#' @returns An S7 external class, i.e. a list with class `S7_external_class`.
#' @export
#' @examples
#' # Referring to a class in another package without taking a hard dependency:
#' Tibble <- new_external_class("tibble", "tbl_df")
#' Tibble
#'
#' # Self-referential class: the `child` property can be another `tree`,
#' # or `NULL` to terminate the chain.
#' tree <- new_class("tree",
#'   package = "mypkg",
#'   properties = list(
#'     child = NULL | new_external_class("mypkg", "tree")
#'   )
#' )
new_external_class <- function(package, name, version = NULL) {
  if (!is_string(package)) {
    stop("`package` must be a string.", call. = FALSE)
  }
  if (!is_string(name)) {
    stop("`name` must be a string.", call. = FALSE)
  }

  out <- list(
    package = package,
    name = name,
    class_name = paste0(package, "::", name),
    version = version
  )
  class(out) <- "S7_external_class"
  out
}

is_external_class <- function(x) {
  inherits(x, "S7_external_class")
}

#' @export
print.S7_external_class <- function(x, ...) {
  cat(
    "<S7_external_class> ",
    x$class_name,
    if (!is.null(x$version)) paste0(" (>= ", x$version, ")"),
    "\n",
    sep = ""
  )
  invisible(x)
}

# Resolve to a real class if the package is loaded (and the optional version
# constraint is met). Returns `NULL` otherwise.
resolve_external_class <- function(x) {
  if (!isNamespaceLoaded(x$package)) {
    return(NULL)
  }
  if (!is.null(x$version) && getNamespaceVersion(x$package) < x$version) {
    return(NULL)
  }

  ns <- asNamespace(x$package)
  if (!exists(x$name, envir = ns, inherits = FALSE)) {
    return(NULL)
  }
  get(x$name, envir = ns, inherits = FALSE)
}
