# Mismatches between a method and its generic (incompatible signature, missing
# arguments, different defaults) are only actionable by the developer of one of
# the packages involved, so we only check methods in development contexts
# (#726, #728). Otherwise, when an upstream package changes a generic, every
# user of an already-installed downstream package would see errors and
# warnings that they can't do anything about.
in_dev <- function(method = NULL, generic = NULL, signature = NULL) {
  if (nzchar(Sys.getenv("DEVTOOLS_LOAD"))) {
    return(TRUE)
  }
  if (is.null(method)) {
    # Nothing to inspect, so load_all() is the only development signal
    return(FALSE)
  }

  method_package <- package_name(method)
  if (is.null(method_package)) {
    # Not in a package, e.g. registered in a script or interactively
    return(TRUE)
  }

  if (is_testing()) {
    # Package tests see end-user behaviour, so that they give the same
    # results under R CMD check (which sets _R_CHECK_PACKAGE_NAME_) as when
    # run locally
    return(FALSE)
  }

  packages <- c(
    method_package,
    package_name(generic),
    signature_packages(signature)
  )
  Sys.getenv("_R_CHECK_PACKAGE_NAME_") %in% packages
}

# Wrapper so that S7's own tests can mock it away when simulating other
# contexts
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

signature_packages <- function(signature) {
  packages <- lapply(signature, function(class) {
    if (is_union(class)) {
      signature_packages(class$classes)
    } else if (is_external_class(class)) {
      class$package
    } else {
      # S7 and S4 classes both store their package as an attribute
      attr(class, "package", exact = TRUE)
    }
  })
  unique(unlist(packages, use.names = FALSE))
}
