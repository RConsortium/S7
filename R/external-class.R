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

class_has_external_class <- function(x) {
  if (is_external_class(x)) {
    TRUE
  } else if (is_union(x)) {
    any(vlapply(x$classes, class_has_external_class))
  } else {
    FALSE
  }
}

signature_has_external_class <- function(signature) {
  any(vlapply(signature, class_has_external_class))
}

class_external_deps <- function(x) {
  if (is_external_class(x)) {
    list(x)
  } else if (is_union(x)) {
    flatten_external_deps(lapply(x$classes, class_external_deps))
  } else {
    list()
  }
}

signature_external_deps <- function(signature) {
  flatten_external_deps(lapply(signature, class_external_deps))
}

flatten_external_deps <- function(x) {
  unlist(x, recursive = FALSE, use.names = FALSE)
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

# Make it mockable
getNamespaceVersion <- NULL

resolve_signature <- function(signature) {
  for (i in seq_along(signature)) {
    signature[[i]] <- resolve_class_req(signature[[i]])
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

# Optional resolution: used by `class_dispatch()` when building the
# dispatch vector, where an unavailable external class should be silently
# skipped.
resolve_external_class_opt <- function(x) {
  if (!dep_available(x)) {
    return(NULL)
  }

  find_external_class(x)
}

find_external_class <- function(x) {
  ns <- asNamespace(x$package)
  if (exists(x$name, envir = ns, inherits = FALSE)) {
    obj <- get(x$name, envir = ns, inherits = FALSE)
    if (is_external_class_match(obj, x)) {
      return(obj)
    }
  }

  for (name in ls(ns, all.names = TRUE)) {
    obj <- get(name, envir = ns, inherits = FALSE)
    if (is_external_class_match(obj, x)) {
      return(obj)
    }
  }

  NULL
}

is_external_class_match <- function(obj, x) {
  is_class(obj) &&
    (identical(S7_class_name(obj), x$class_name) ||
      (identical(obj@name, x$name) &&
        (is.null(obj@package) || identical(obj@package, x$package))))
}

# Required resolution: used when registering a method, when extending
# (since the child constructor inlines the parent arguments) and when
# constructing or validating an instance.
resolve_external_class_req <- function(x) {
  prefix <- sprintf("Can't find external class <%s>:\n", x$class_name)
  if (!requireNamespace(x$package, quietly = TRUE)) {
    stop2(
      paste0(prefix, sprintf("* Package '%s' is not installed.", x$package)),
      call = NULL
    )
  }

  if (!is.null(x$version) && getNamespaceVersion(x$package) < x$version) {
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
        sprintf("* Packages '%s' doesn't contain '%s'.", x$package, x$name)
      ),
      call = NULL
    )
  }
  class
}
