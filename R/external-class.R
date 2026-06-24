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
#' External classes can not currently be used as parents in [new_class()].
#' We hope to relax that restriction in the near future.
#'
#' @param package Package the class is defined in.
#' @param name Name of the class, as a string.
#' @inheritParams new_external_generic version
#' @returns An S7 external class, i.e. a list with S3 class `S7_external_class`.
#' @export
#' @examples
#' # Refer to an S7 class in another package without taking a hard dependency:
#' TheirClass <- new_external_class("theirpkg", "TheirClass")
#' TheirClass
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
    stop2("`package` must be a string.")
  }
  if (!is_string(name)) {
    stop2("`name` must be a string.")
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
  isNamespaceLoaded(dep$package) && dep_version_ok(dep)
}

dep_version_ok <- function(dep) {
  is.null(dep$version) || getNamespaceVersion(dep$package) >= dep$version
}

# Resolve signature if all external classes are availabe
resolve_signature_available <- function(signature, package = NULL) {
  deps <- signature_deps(signature)
  all_available <- all(vlapply(deps, dep_available))

  if (all_available) {
    signature <- resolve_signature(signature, package)
  }
  signature
}

resolve_signature <- function(signature, package = NULL) {
  for (i in seq_along(signature)) {
    x <- signature[[i]]
    if (is_external_class(x)) {
      signature[[i]] <- resolve_external_class_req(x, package)
    } else if (is_union(x)) {
      signature[[i]] <- do.call(
        new_union,
        resolve_signature(x$classes, package)
      )
    }
  }
  signature
}


# Errors if the external class can't be resolved
# * The package isn't installed
# * The package is too old
# * The package doesn't export the appropriate S7 class
#
# A package can refer to its own (possibly unexported) classes by passing its
# own name as `package`; references to other packages must be exported.
#
# Used wherever we need the real class: registering or looking up methods,
# checking property overrides in a subclass, and constructing or validating
# an instance.
resolve_external_class_req <- function(x, package = NULL) {
  error_class <- "S7_error_external_class_unresolved"
  error_header <- sprintf("Can't find external class <%s>:", x$class_name)
  if (!requireNamespace(x$package, quietly = TRUE)) {
    stop2(
      c(error_header, sprintf("* Package '%s' is not installed.", x$package)),
      call = NULL,
      class = error_class
    )
  }

  if (!dep_version_ok(x)) {
    stop2(
      c(
        error_header,
        sprintf(
          "* Package '%s' needs version %s, but only %s is available.",
          x$package,
          x$version,
          getNamespaceVersion(x$package)
        )
      ),
      call = NULL,
      class = error_class
    )
  }

  ns <- asNamespace(x$package)
  same_package <- identical(package, x$package)
  if (same_package || x$name %in% getNamespaceExports(ns)) {
    obj <- get0(x$name, envir = ns, inherits = FALSE)
  } else {
    obj <- NULL
  }

  is_match <- is_class(obj) &&
    identical(obj@name, x$name) &&
    identical(obj@package, x$package)

  if (!is_match) {
    verb <- if (same_package) "bind" else "export"
    stop2(
      sprintf(
        "Package '%s' must %s `%s` as the S7 class <%s>.",
        x$package,
        verb,
        x$name,
        x$class_name
      ),
      call = NULL,
      class = error_class
    )
  }

  obj
}
