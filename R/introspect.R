#' Find S7 classes and generics in an environment
#'
#' @description
#' * `S7_classes()` returns the names of S7 classes defined in `env`.
#' * `S7_generics()` returns the names of S7 generics defined in `env`.
#'
#' @param env An environment. Defaults to the caller's environment.
#'   To inspect a package, pass `asNamespace("pkg")`; to inspect the global
#'   environment, pass `globalenv()`.
#' @returns A character vector of names.
#' @export
#' @examples
#' # List S7 classes exported by the S7 package itself
#' S7_classes(asNamespace("S7"))
#' S7_generics(asNamespace("S7"))
S7_classes <- function(env = parent.frame()) {
  find_objects(env, is_class)
}

#' @export
#' @rdname S7_classes
S7_generics <- function(env = parent.frame()) {
  find_objects(env, is_S7_generic)
}

#' List S7 methods
#'
#' List the methods registered on an S7 `generic`, or the methods registered
#' for a given `class` across all S7 generics defined in attached packages.
#'
#' @param generic An S7 generic.
#' @param class A class specification (anything accepted by [as_class()]).
#'   When supplied, every S7 generic in every attached package is searched
#'   for methods with this class in their signature.
#' @returns A data frame with one row per matching method and columns:
#'
#'   * `generic`: the generic's name.
#'   * `package`: the package the generic is defined in, or `NA` for generics
#'     found in the global environment (or when `generic` is supplied
#'     directly).
#'   * `signature`: human-readable description of the dispatch signature.
#'   * `method`: a string giving the `method()` call that retrieves the
#'     method.
#' @export
#' @examples
#' Foo <- new_class("Foo", package = NULL)
#' Bar <- new_class("Bar", package = NULL)
#' my_gen <- new_generic("my_gen", "x")
#' method(my_gen, Foo) <- function(x) "foo"
#' method(my_gen, Bar) <- function(x) "bar"
#'
#' S7_methods(generic = my_gen)
#' S7_methods(class = Foo)
S7_methods <- function(generic = NULL, class = NULL) {
  if (!is.null(generic)) {
    if (!is_S7_generic(generic)) {
      stop("`generic` must be an S7 generic.")
    }
    generics <- list(list(generic = generic, package = NA_character_))
  } else {
    generics <- attached_generics()
  }

  if (!is.null(class)) {
    target <- class_register(as_class(class))
  } else {
    target <- NULL
  }

  rows <- lapply(generics, function(g) {
    generic_method_rows(g$generic, g$package, target)
  })
  do.call(rbind, rows)
}

# Per-generic helper: turn the generic's registered methods into a data
# frame, optionally filtering to those whose signature contains `target`.
generic_method_rows <- function(
  generic,
  package = NA_character_,
  target = NULL
) {
  ms <- methods(generic)
  if (!is.null(target)) {
    ms <- Filter(x = ms, function(m) {
      any(vcapply(m@signature, class_register) == target)
    })
  }

  data.frame(
    generic = rep(generic@name, length(ms)),
    package = rep(package, length(ms)),
    signature = vcapply(ms, function(m) {
      paste0(vcapply(m@signature, class_desc), collapse = ", ")
    }),
    method = vcapply(ms, \(m) method_signature(generic, m@signature))
  )
}

# All S7 generics reachable from attached packages and the global env,
# each tagged with the package it was found in (`NA` for the global env).
attached_generics <- function() {
  out <- list()
  for (env in attached_envs()) {
    package <- env_package(env)
    for (generic in unname(find_matches(env, is_S7_generic))) {
      out[[length(out) + 1L]] <- list(generic = generic, package = package)
    }
  }
  out
}

env_package <- function(env) {
  if (identical(env, globalenv())) {
    NA_character_
  } else {
    sub("^package:", "", environmentName(env))
  }
}

attached_envs <- function() {
  envs <- search()
  pkgs <- envs[grepl("^package:", envs)]
  pkgs <- setdiff(pkgs, "package:base")

  c(lapply(pkgs, as.environment), globalenv())
}

find_objects <- function(env, predicate) {
  names(find_matches(env, predicate))
}

# Named list of objects in `env` satisfying `predicate`.
find_matches <- function(env, predicate) {
  if (isNamespace(env)) {
    # Not attached; use exported values
    names <- getNamespaceExports(env)
  } else {
    # Attached or global; use all values
    names <- ls(envir = env)
  }

  objs <- mget(names, envir = env, inherits = FALSE)
  Filter(predicate, objs)
}
