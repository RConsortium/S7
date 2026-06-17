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
  if (is_S7_generic(x)) {
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

registrar <- function(generic, signature, method, env) {
  # Force all arguments
  generic
  signature
  method
  env

  function(...) {
    ns <- asNamespace(generic$package)
    if (external_generic_version_ok(generic, ns)) {
      if (!exists(generic$name, envir = ns, inherits = FALSE)) {
        msg <- sprintf(
          "[S7] Failed to find generic %s() in package %s",
          generic$name,
          generic$package
        )
        warning(msg, call. = FALSE)
      } else {
        generic_fun <- get(generic$name, envir = ns, inherits = FALSE)
        if (is_S7_generic(generic_fun)) {
          previous <- external_methods_capture_previous(
            generic,
            signature,
            method,
            generic_fun,
            packageName(env)
          )
          if (!is.null(previous)) {
            external_methods_set_previous(
              packageName(env),
              generic,
              signature,
              previous
            )
          }
        }
        register_method(generic_fun, signature, method, env, package = NULL)
      }
    }
  }
}

external_methods_reset <- function(package) {
  S7_methods_table(package) <- list()
  invisible()
}

external_methods_add <- function(
  package,
  generic,
  signature,
  method,
  previous = NULL
) {
  existing <- external_methods_find(package, generic, signature)
  previous <- external_methods_merge_previous(previous, existing$previous)

  # Remove any existing entries
  external_methods_remove(package, generic, signature)

  entry <- list(
    generic = generic,
    signature = signature,
    method = method
  )
  if (!is.null(previous)) {
    entry$previous <- previous
  }

  tbl <- S7_methods_table(package)
  append1(tbl) <- entry

  S7_methods_table(package) <- tbl
  invisible()
}

external_methods_find <- function(package, generic, signature) {
  tbl <- S7_methods_table(package)
  for (x in tbl) {
    if (identical(x$generic, generic) && identical(x$signature, signature)) {
      return(x)
    }
  }

  NULL
}

external_methods_merge_previous <- function(previous, existing) {
  if (is.null(previous)) {
    return(existing)
  }
  if (is.null(existing)) {
    return(previous)
  }

  n <- max(length(previous), length(existing))
  length(previous) <- n
  length(existing) <- n
  missing <- vlapply(previous, is.null)
  previous[missing] <- existing[missing]
  previous
}

external_methods_remove <- function(package, generic, signature) {
  tbl <- S7_methods_table(package)
  if (length(tbl) == 0) {
    return(invisible())
  }

  keep <- !vlapply(tbl, function(x) {
    identical(x$generic, generic) && identical(x$signature, signature)
  })
  S7_methods_table(package) <- tbl[keep]
  invisible()
}

external_methods_capture_previous <- function(
  generic,
  signature,
  method,
  generic_fun,
  package = NULL
) {
  signatures <- flatten_signature(signature)
  previous <- vector("list", length(signatures))
  found <- FALSE

  for (i in seq_along(signatures)) {
    sig <- signatures[[i]]
    current <- generic_get_method(generic_fun, sig)
    own <- S7_method_for_signature(method, generic_fun, sig, package = package)

    restored <- external_method_restoration(current, generic, sig, generic_fun)
    if (!identical(current, own) && !is.null(restored)) {
      previous[[i]] <- restored
      found <- TRUE
    }
  }

  if (found) previous else NULL
}

external_methods_set_previous <- function(package, generic, signature, previous) {
  tbl <- S7_methods_table(package)
  for (i in seq_along(tbl)) {
    x <- tbl[[i]]
    if (identical(x$generic, generic) && identical(x$signature, signature)) {
      tbl[[i]]$previous <- external_methods_merge_previous(previous, x$previous)
      S7_methods_table(package) <- tbl
      return(invisible())
    }
  }

  invisible()
}

external_method_restoration <- function(method, generic, signature, generic_fun) {
  if (is.null(method)) {
    return(NULL)
  }
  if (is_S7_method_from_package(method, generic$package)) {
    return(method)
  }

  package <- S7_method_package(method)
  if (is.null(package) || !isNamespaceLoaded(package)) {
    return(NULL)
  }

  tbl <- S7_methods_table(package)
  for (x in tbl) {
    if (!external_generics_match(x$generic, generic)) {
      next
    }

    signatures <- flatten_signature(x$signature)
    for (i in seq_along(signatures)) {
      sig <- signatures[[i]]
      own <- S7_method_for_signature(
        x$method,
        generic_fun,
        sig,
        package = package
      )
      if (
        identical(signature, sig) &&
          identical(method, own) &&
          length(x$previous) >= i
      ) {
        return(x$previous[[i]])
      }
    }
  }

  NULL
}

external_generics_match <- function(x, y) {
  identical(x$package, y$package) &&
    identical(x$name, y$name) &&
    identical(x$dispatch_args, y$dispatch_args)
}

is_S7_method_from_package <- function(method, package) {
  inherits(method, "S7_method") &&
    identical(S7_method_package(method), package)
}

S7_method_package <- function(method) {
  if (!inherits(method, "S7_method")) {
    return(NULL)
  }

  attr(method, "S7_package", TRUE) %||% packageName(environment(method))
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
