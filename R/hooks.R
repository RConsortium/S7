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

  hooks_remove(package) # always start from a clean slate
  hooks <- hooks_add(package)
  hooks_run_loaded(hooks) # run hooks for loaded packages

  invisible()
}

#' @export
#' @rdname S7_on_load
S7_on_unload <- function() {
  S7_on_unload_(parent.frame())
}

S7_on_unload_ <- function(env) {
  package <- packageName(env)
  hooks_remove(package)

  tbl <- S7_methods_table(package)
  for (x in tbl) {
    if (!isNamespaceLoaded(x$generic$package)) {
      next
    }

    ns <- asNamespace(x$generic$package)
    generic <- get0(x$generic$name, envir = ns, inherits = FALSE)
    # Methods registered for S3 and S4 generics can't be unregistered yet
    if (is_S7_generic(generic)) {
      unregister_own_S7_method(generic, x$signature, x$method)
    }
  }

  invisible()
}

# Add a hook for each method that registers it when its generic's package is
# loaded. Returns the added hooks, named by the package they're attached to.
hooks_add <- function(package) {
  ns <- asNamespace(package)
  tbl <- S7_methods_table(package)
  pkgs <- vcapply(tbl, function(x) x$generic$package)

  hooks <- lapply(tbl, function(x) {
    register <- registrar(x$generic, x$signature, x$method, ns)
    hook <- S7_hook(register, package)
    setHook(packageEvent(x$generic$package, "onLoad"), hook)
    hook
  })
  names(hooks) <- pkgs
  hooks_packages(package) <- union(hooks_packages(package), pkgs)
  hooks
}

# Remove our hooks for `package`.
hooks_remove <- function(package) {
  tbl <- S7_methods_table(package)
  pkgs <- unique(c(
    hooks_packages(package),
    vcapply(tbl, function(x) x$generic$package)
  ))

  for (pkg in pkgs) {
    event <- packageEvent(pkg, "onLoad")
    hooks <- getHook(event)
    ours <- vlapply(hooks, is_S7_hook, package = package)
    if (any(ours)) {
      setHook(event, hooks[!ours], action = "replace")
    }
  }
  hooks_packages(package) <- character()
  invisible()
}

hooks_run_loaded <- function(hooks) {
  is_loaded <- vlapply(names(hooks), isNamespaceLoaded)
  for (hook in hooks[is_loaded]) {
    hook()
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

hooks_packages <- function(package) {
  ns <- asNamespace(package)
  tbl <- ns[[".__S3MethodsTable__."]]
  attr(tbl, "S7hooks") %||% character()
}
`hooks_packages<-` <- function(package, value) {
  ns <- asNamespace(package)
  tbl <- ns[[".__S3MethodsTable__."]]
  attr(tbl, "S7hooks") <- value
  invisible()
}

unregister_own_S7_method <- function(generic, signature, method) {
  signatures <- flatten_signature(signature)
  for (sig in signatures) {
    current <- generic_get_method(generic, sig)
    own <- S7_method(method, generic = generic, signature = sig)
    if (is.null(attr(own, "name", TRUE))) {
      attr(own, "name") <- as.name(method_signature(generic, sig))
    }
    if (identical(current, own)) {
      generic_remove_method(generic, sig)
    }
  }
  invisible()
}
