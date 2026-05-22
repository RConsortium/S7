#' List S7 classes and generics in an environment
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

find_objects <- function(env, predicate) {
  if (isNamespace(env)) {
    # Not attached; use exported values
    names <- getNamespaceExports(env)
  } else {
    # Attached or global; use all values
    names <- ls(envir = env)
  }

  Filter(\(name) predicate(get(name, envir = env, inherits = FALSE)), names)
}
