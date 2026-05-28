#' Package hooks for S7 methods
#'
#' @description
#' When using S7 in a package, add two hooks to your `zzz.R`:
#'
#' * Call `methods_register()` from `.onLoad()`. This is S7's way of
#'   registering methods, rather than using `NAMESPACE` directives like S3 and
#'   S4 do. It ensures that methods for generics (S3, S4, and S7) defined in
#'   other packages are registered as needed when your package is loaded. This
#'   is only strictly necessary if you register methods for generics in other
#'   packages, but there's no harm in always including it and it ensures you
#'   won't forget later.
#'
#' * Call `S7_on_build()` at the top level (i.e. *not* inside `.onLoad()`).
#'   This avoids embedding copies of external, S3, and S4 generics in your
#'   package when you use `method<-`.
#'
#' See `vignette("packages")` for more details.
#'
#' @importFrom utils getFromNamespace packageName
#' @returns Nothing; both functions are called for their side-effects.
#' @examples
#' # In zzz.R:
#' .onLoad <- function(...) {
#'   S7::methods_register()
#' }
#' S7::S7_on_build()
#' @export
methods_register <- function() {
  package <- packageName(parent.frame())
  ns <- topenv(parent.frame())
  # TODO?: check/enforce that methods_register() is being called from .onLoad()

  tbl <- S7_methods_table(package)

  for (x in tbl) {
    register <- registrar(x$generic, x$signature, x$method, ns)

    if (isNamespaceLoaded(x$generic$package)) {
      register()
    }
    setHook(packageEvent(x$generic$package, "onLoad"), register)
  }

  invisible()
}

#' @export
#' @rdname methods_register
S7_on_build <- function() {
  strip_generic_sentinels(topenv(parent.frame()))
}

strip_generic_sentinels <- function(ns) {
  for (name in ls(ns, all.names = TRUE)) {
    if (is_generic_sentinel(get0(name, envir = ns, inherits = FALSE))) {
      rm(list = name, envir = ns)
    }
  }
  invisible()
}

generic_sentinel <- function(generic) {
  external <- as_external_generic(generic)
  class(external) <- c("S7_generic_sentinel", "S7_external_generic")
  external
}

is_generic_sentinel <- function(x) inherits(x, "S7_generic_sentinel")
