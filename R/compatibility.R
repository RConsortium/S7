# Where needed, attach an environment containing @ that works with S7
activate_backward_compatiblility <- function() {
  if (getRversion() < "4.3.0" && !"S7_at" %in% search()) {
    args <- list(list("@" = `@`), name = "S7_at", warn.conflicts = FALSE)
    do.call("attach", args)
  }
  invisible()
}

bind_conflict_packages <- c("data.table", "rlang")

set_attached_bind <- function(env, value) {
  get("unlockBinding", baseenv())(":=", env)
  defer(lockBinding(":=", env))
  env[[":="]] <- value
}

activate_bind_compatibility <- function() {
  for (package in bind_conflict_packages) {
    # These rules are session-wide. If S7 is detached, a package attached
    # later may remain without its error-only := export on the search path.
    rule <- conflictRules(package)
    conflictRules(
      package,
      mask.ok = rule$mask.ok,
      exclude = union(rule$exclude, ":=")
    )

    attached <- paste0("package:", package)
    if (!attached %in% search()) {
      next
    }

    env <- as.environment(attached)
    if (
      exists(":=", envir = env, inherits = FALSE) &&
        identical(env[[":="]], getExportedValue(package, ":="))
    ) {
      # These exports are erroring sentinels for package-specific NSE syntax.
      # An identical binding is not reported as a conflict by library().
      set_attached_bind(env, `:=`)
    }
  }

  invisible()
}

restore_attached_bindings <- function() {
  for (package in bind_conflict_packages) {
    attached <- paste0("package:", package)
    if (!attached %in% search()) {
      next
    }

    env <- as.environment(attached)
    if (identical(env[[":="]], `:=`)) {
      set_attached_bind(env, getExportedValue(package, ":="))
    }
  }

  invisible()
}

#' @aliases @
#' @usage NULL
#' @rawNamespace if (getRversion() < "4.3.0") export(`@`)
#' @name prop
NULL

if (getRversion() < "4.3.0") {
  `@` <- function(object, name) {
    if (inherits(object, "S7_object")) {
      name <- as.character(substitute(name))
      prop(object, name)
    } else {
      name <- substitute(name)
      do.call(base::`@`, list(object, name))
    }
  }
}
