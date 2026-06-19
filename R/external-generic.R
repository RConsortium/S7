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
#' can be registered at run time, as needed, using [S7_on_load()].
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
#' MyClass := new_class()
#'
#' your_generic <- new_external_generic("stats", "median", "x")
#' method(your_generic, MyClass) <- function(x) "Hi!"
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

as_external_generic <- function(x, env = parent.frame()) {
  if (is_generic_sentinel(x)) {
    class(x) <- "S7_external_generic"
    x
  } else if (is_S7_generic(x)) {
    pkg <- package_name(x)
    new_external_generic(pkg, x@name, x@dispatch_args)
  } else if (is_external_generic(x)) {
    x
  } else if (is_S3_generic(x)) {
    pkg <- package_name(x$generic)
    new_external_generic(pkg, x$name, "__S3__")
  } else if (is_S4_generic(x)) {
    pkg <- S4_package_name(x, env)
    new_external_generic(pkg, as.vector(x@generic), x@signature)
  }
}

#' @export
print.S7_external_generic <- function(x, ...) {
  cat(
    "<S7_external_generic> ",
    x$package,
    "::",
    x$name,
    "(",
    paste(x$dispatch_args, collapse = ", "),
    ")",
    if (!is.null(x$version)) paste0(" (>= ", x$version, ")"),
    "\n",
    sep = ""
  )
  invisible(x)
}

is_external_generic <- function(x) {
  inherits(x, "S7_external_generic")
}

external_generic_available <- function(generic) {
  is_external_generic(generic) &&
    isNamespaceLoaded(generic$package) &&
    external_generic_version_ok(generic, asNamespace(generic$package))
}

external_generic_version_ok <- function(generic, ns) {
  stopifnot(is_external_generic(generic), is.environment(ns))

  is.null(generic$version) || getNamespaceVersion(ns) >= generic$version
}

registrar <- function(
  deps,
  generic,
  signature,
  method,
  env,
  on_load_package = NULL
) {
  # Force all arguments
  deps
  generic
  signature
  method
  env
  on_load_package

  function(...) {
    if (!dep_available(generic)) {
      return(invisible())
    }

    generic_fun <- resolve_generic(generic)
    if (is.null(generic_fun)) {
      return(invisible())
    }

    signatures <- list()
    for (sig in flatten_signature(signature)) {
      if (!registrar_signature_needs_package(sig, generic, on_load_package)) {
        next
      }

      sig_deps <- signature_external_deps(sig)
      if (!all(vlapply(sig_deps, dep_available))) {
        next
      }

      append1(signatures) <- resolve_signature(sig)
    }

    for (sig in signatures) {
      register_method(generic_fun, sig, method, env, package = NULL)
    }

    invisible()
  }
}

registrar_signature_needs_package <- function(signature, generic, package) {
  if (is.null(package) || identical(package, generic$package)) {
    return(TRUE)
  }

  deps <- signature_external_deps(signature)
  any(vlapply(deps, function(dep) identical(dep$package, package)))
}

# Collects all external dependencies (the generic + any external classes)
# into a single list. Each entry has at minimum `package` + `version`.
method_deps <- function(generic, signature) {
  c(list(generic), signature_external_deps(signature))
}
method_deps_packages <- function(deps) {
  unique(vcapply(deps, function(dep) dep$package))
}

resolve_generic <- function(generic) {
  ns <- asNamespace(generic$package)
  if (exists(generic$name, envir = ns, inherits = FALSE)) {
    get(generic$name, envir = ns, inherits = FALSE)
  } else {
    warning(
      sprintf(
        "[S7] Failed to find generic %s() in package %s",
        generic$name,
        generic$package
      ),
      call. = FALSE
    )
    NULL
  }
}

external_methods_add <- function(
  package,
  generic,
  signature,
  method
) {
  # Remove any existing entries
  removed <- external_methods_remove(package, generic, signature)

  entry <- list(
    generic = generic,
    signature = signature,
    method = method
  )

  tbl <- S7_methods_table(package)
  append1(tbl) <- entry

  S7_methods_table(package) <- tbl
  invisible(removed)
}

external_methods_remove <- function(package, generic, signature) {
  tbl <- S7_methods_table(package)
  if (length(tbl) == 0) {
    return(invisible(list()))
  }

  active <- hooks_active(package)
  new_tbl <- list()
  removed <- list()
  rehook <- list()

  for (x in tbl) {
    if (!identical(x$generic, generic)) {
      append1(new_tbl) <- x
      next
    }

    change <- external_method_signature_remove(x$signature, signature)
    if (is.null(change)) {
      append1(new_tbl) <- x
      next
    }

    for (sig in change$removed) {
      removed_x <- x
      removed_x$signature <- sig
      append1(removed) <- removed_x
    }
    hooks_remove_method(package, x)

    for (sig in change$remaining) {
      remaining_x <- x
      remaining_x$signature <- sig
      append1(new_tbl) <- remaining_x
      append1(rehook) <- remaining_x
    }
  }

  `S7_methods_table<-`(package, new_tbl)
  if (length(rehook) > 0 && active) {
    for (x in rehook) {
      hook_set_and_run(package, x)
    }
  }

  invisible(removed)
}

external_method_signature_remove <- function(x, y) {
  if (length(x) != length(y)) {
    return(NULL)
  }

  x_signatures <- flatten_signature(x)
  y_signatures <- flatten_signature(y)
  removed <- list()
  remaining <- list()

  for (x_sig in x_signatures) {
    matched <- any(vlapply(y_signatures, function(y_sig) {
      external_method_signature_matches(x_sig, y_sig)
    }))
    if (matched) {
      append1(removed) <- new_signature(x_sig)
    } else {
      append1(remaining) <- new_signature(x_sig)
    }
  }

  if (length(removed) == 0) {
    return(NULL)
  }

  list(removed = removed, remaining = remaining)
}

external_method_signature_matches <- function(x, y) {
  if (identical(x, y)) {
    return(TRUE)
  }
  if (length(x) != length(y)) {
    return(FALSE)
  }

  all(vlapply(seq_along(x), function(i) {
    external_method_class_matches(x[[i]], y[[i]])
  }))
}

external_method_class_matches <- function(x, y) {
  if (identical(x, y)) {
    return(TRUE)
  }

  if (is_external_class(x) && is_class(y)) {
    return(is_external_class_match(y, x))
  }
  if (is_class(x) && is_external_class(y)) {
    return(is_external_class_match(x, y))
  }
  if (is_external_class(x) && is_external_class(y)) {
    return(identical(x$class_name, y$class_name))
  }

  FALSE
}

# Store external methods in an attribute of the S3 methods table since
# this mutable object is present in all packages.

S7_methods_table <- function(package) {
  ns <- asNamespace(package)
  tbl <- ns[[".__S3MethodsTable__."]]
  attr(tbl, "S7methods") %||% list()
}
`S7_methods_table<-` <- function(package, value) {
  ns <- asNamespace(package)
  tbl <- ns[[".__S3MethodsTable__."]]
  attr(tbl, "S7methods") <- value
  invisible()
}
