#' Classes in other packages
#'
#' @description
#' An external class is a lightweight placeholder for an S7 class defined in
#' another package (or in your own package and needed before it's fully
#' defined). It carries only the package and class name, and is resolved to
#' the real S7 class when needed.
#'
#' External classes are useful in two situations:
#'
#' * To register a method for a generic in your package, dispatching on a class
#'   from a soft dependency. The method will be registered when `pkg` is loaded
#'   (using the same machinery as [new_external_generic()]).
#'
#'   ```R
#'   SomeClass <- new_external_class("pkg", "SomeClass")
#'   method(my_generic, SomeClass) <- ...
#'   ```
#'
#' * To refer to a class that hasn't been defined yet, such as a
#'   self-referential or mutually recursive class.
#'
#'   ```R
#'   tree_stub <- new_external_class("mypkg", "tree")
#'   new_class("tree", properties = list(child = NULL | tree_stub))
#'   ```
#'
#' Make sure to call [S7_on_load()] in your package's `.onLoad()` so that
#' deferred method registrations fire when the relevant package is loaded.
#'
#' @param package Package the class is defined in.
#' @param name Name of the class, as a string.
#' @inheritParams new_external_generic version
#' @returns An S7 external class, i.e. a list with S3 class `S7_external_class`.
#' @export
#' @examples
#' # Refer to a class in another package without taking a hard dependency:
#' Tibble <- new_external_class("tibble", "tbl_df")
#' Tibble
#'
#' # Self-referential class: the `child` property can be another `tree`,
#' # or `NULL` to terminate the chain.
#' tree_stub <- new_external_class("mypkg", "tree")
#' tree <- new_class(
#'   name = "tree",
#'   package = "mypkg",
#'   properties = list(child = NULL | tree_stub)
#' )
new_external_class <- function(package, name, version = NULL) {
  if (!is_string(package)) {
    stop("`package` must be a string.")
  }
  if (!is_string(name)) {
    stop("`name` must be a string.")
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

dep_available <- function(dep) {
  isNamespaceLoaded(dep$package) &&
    (is.null(dep$version) || getNamespaceVersion(dep$package) >= dep$version)
}

resolve_signature <- function(signature) {
  for (i in seq_along(signature)) {
    if (is_external_class(signature[[i]])) {
      signature[[i]] <- resolve_external_class_req(signature[[i]])
    }
  }
  signature
}

# Resolve to the real class if the package is loaded (and the optional version
# constraint is met). Returns `NULL` otherwise.
resolve_external_class_opt <- function(x) {
  if (!dep_available(x)) {
    return(NULL)
  }

  ns <- asNamespace(x$package)
  if (!exists(x$name, envir = ns, inherits = FALSE)) {
    return(NULL)
  }
  get(x$name, envir = ns, inherits = FALSE)
}

# Resolve to the real class, loading the package if needed, erroring with a
# specific message for each failure mode.
resolve_external_class_req <- function(x) {
  prefix <- sprintf("Can't find external class <%s>", x$class_name)
  if (!requireNamespace(x$package, quietly = TRUE)) {
    stop(
      sprintf(
        "%s: package '%s' is not installed.",
        prefix,
        x$package
      ),
      call. = FALSE
    )
  }

  if (!is.null(x$version) && getNamespaceVersion(x$package) < x$version) {
    stop(
      sprintf(
        "%s: package '%s' is version %s, but >= %s is required.",
        prefix,
        x$package,
        getNamespaceVersion(x$package),
        x$version
      ),
      call. = FALSE
    )
  }

  ns <- asNamespace(x$package)
  if (!exists(x$name, envir = ns, inherits = FALSE)) {
    stop(
      sprintf(
        "%s: '%s' is not found in package '%s'.",
        prefix,
        x$name,
        x$package
      ),
      call. = FALSE
    )
  }
  get(x$name, envir = ns, inherits = FALSE)
}
