#' Package hooks for S7 methods
#'
#' @description
#' When using S7 in a package, add three hooks to your `zzz.R`:
#'
#' * Call `S7_on_load()` from `.onLoad()`. This is S7's way of
#'   registering methods, rather than using `NAMESPACE` directives like S3 and
#'   S4 do. It ensures that methods for generics (S3, S4, and S7) defined in
#'   other packages are registered as needed when your package is loaded. This
#'   is only strictly necessary if you register methods for generics in other
#'   packages, but there's no harm in always including it and it ensures you
#'   won't forget later.
#'
#' * Call `S7_on_unload()` from `.onUnload()`. This undoes the work of
#'   `S7_on_load()`: it unregisters the methods that your package registered
#'   for S7 generics in other packages and removes any hooks that
#'   `S7_on_load()` added.
#'
#' * Call `S7_on_build()` at the top level (i.e. *not* inside `.onLoad()`)
#'   after all method registration is complete. This avoids embedding copies
#'   of external generics in your package when you use `method<-`.
#'
#' `S7_on_load()` was previously known as `methods_register()`. This function is
#' retained for backward compatibility but new code should use `S7_on_load()`.
#'
#' See `vignette("packages")` for more details.
#'
#' @importFrom utils getFromNamespace packageName
#' @returns Nothing; these functions are called for their side-effects.
#' @examples
#' # In zzz.R:
#' .onLoad <- function(...) {
#'   S7::S7_on_load()
#' }
#' .onUnload <- function(...) {
#'   S7::S7_on_unload()
#' }
#' S7::S7_on_build()
#' @export
S7_on_load <- function() {
  S7_on_load_(parent.frame())
}

#' @export
#' @rdname S7_on_load
methods_register <- function() {
  S7_on_load_(parent.frame())
}

S7_on_load_ <- function(env) {
  package <- packageName(env)
  ns <- topenv(env)
  # TODO?: check/enforce that S7_on_load() is being called from .onLoad()

  tbl <- S7_methods_table(package)

  # Remove any hooks left behind by a previous load of this package
  registrar_hooks_remove(tbl, package)

  for (x in tbl) {
    register <- registrar(x$generic, x$signature, x$method, ns, package)

    if (isNamespaceLoaded(x$generic$package)) {
      register()
    }
    setHook(packageEvent(x$generic$package, "onLoad"), register)
  }

  invisible()
}

#' @export
#' @rdname S7_on_load
S7_on_unload <- function() {
  S7_on_unload_(parent.frame())
}

S7_on_unload_ <- function(env) {
  package <- packageName(env)

  tbl <- S7_methods_table(package)
  registrar_hooks_remove(tbl, package)

  for (x in tbl) {
    if (!isNamespaceLoaded(x$generic$package)) {
      next
    }

    generic <- get0(
      x$generic$name,
      envir = asNamespace(x$generic$package),
      inherits = FALSE
    )
    # Methods registered for S3 and S4 generics can't be unregistered
    if (is_S7_generic(generic)) {
      unregister_S7_method(generic, x$signature)
    }
  }

  invisible()
}

# Remove the hooks that S7_on_load_() added on behalf of `package`
registrar_hooks_remove <- function(tbl, package) {
  pkgs <- unique(vcapply(tbl, function(x) x$generic$package))

  for (pkg in pkgs) {
    event <- packageEvent(pkg, "onLoad")
    hooks <- getHook(event)
    ours <- vlapply(hooks, is_S7_hook, package = package)
    if (any(ours)) {
      setHook(event, hooks[!ours], action = "replace")
    }
  }

  invisible()
}

#' @export
#' @rdname S7_on_load
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


# Tag our hooks so we can remove later
S7_hook <- function(fun, package) {
  attr(fun, "S7_package") <- package
  class(fun) <- "S7_hook"
  fun
}
is_S7_hook <- function(x, package) {
  inherits(x, "S7_hook") && identical(attr(x, "S7_package", TRUE), package)
}
