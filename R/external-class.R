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

class_external_deps <- function(x) {
  if (is_external_class(x)) {
    list(x)
  } else if (is_union(x)) {
    unlist(lapply(x$classes, class_external_deps), recursive = FALSE)
  } else {
    list()
  }
}

signature_external_deps <- function(signature) {
  unlist(lapply(signature, class_external_deps), recursive = FALSE)
}

external_deps_resolvable <- function(deps) {
  all(vlapply(deps, function(dep) {
    dep_available(dep) && !is.null(find_external_class(dep))
  }))
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

# Make it mockable
getNamespaceVersion <- NULL

resolve_signature <- function(signature) {
  for (i in seq_along(signature)) {
    signature[i] <- list(resolve_class_req(signature[[i]]))
  }
  signature
}

resolve_class_req <- function(x) {
  if (is_external_class(x)) {
    resolve_external_class_req(x)
  } else if (is_union(x)) {
    do.call(new_union, lapply(x$classes, resolve_class_req))
  } else {
    x
  }
}

find_external_class <- function(x) {
  ns <- asNamespace(x$package)
  obj <- get0(x$name, envir = ns, inherits = FALSE)
  if (is_external_class_match(obj, x)) {
    obj
  } else {
    NULL
  }
}

is_external_class_match <- function(obj, x) {
  is_class(obj) &&
    identical(obj@name, x$name) &&
    identical(obj@package, x$package)
}

# Required resolution: errors if the external class can't be resolved (e.g.
# its package isn't loaded). Used wherever we need the real class: registering
# or looking up methods, checking property overrides in a subclass, and
# constructing or validating an instance.
resolve_external_class_req <- function(x) {
  prefix <- sprintf("Can't find external class <%s>:\n", x$class_name)
  if (!requireNamespace(x$package, quietly = TRUE)) {
    stop2(
      paste0(prefix, sprintf("* Package '%s' is not installed.", x$package)),
      call = NULL
    )
  }

  if (!dep_version_ok(x)) {
    stop2(
      paste0(
        prefix,
        sprintf(
          "* Package '%s' needs version %s, but only %s is available.",
          x$package,
          x$version,
          getNamespaceVersion(x$package)
        )
      ),
      call = NULL
    )
  }

  class <- find_external_class(x)
  if (is.null(class)) {
    stop2(
      paste0(
        prefix,
        sprintf(
          paste0(
            "* Package '%s' must bind an S7 class to `%s` with ",
            "@name '%s' and @package '%s'."
          ),
          x$package,
          x$name,
          x$name,
          x$package
        )
      ),
      call = NULL
    )
  }
  class
}
