# Where needed, attach an environment containing @ that works with S7
activate_backward_compatiblility <- function() {
  if (getRversion() < "4.3.0" && !"S7_at" %in% search()) {
    args <- list(list("@" = `@`), name = "S7_at", warn.conflicts = FALSE)
    do.call("attach", args)
  }
  invisible()
}

search_has_bind_conflict <- function(pkgname) {
  pkg <- paste0("package:", pkgname)
  env <- as.environment(pkg)
  bind <- env[[":="]]
  where <- setdiff(search(), c(pkg, "Autoloads", "CheckExEnv"))

  for (pos in where) {
    other <- as.environment(pos)
    if (!exists(":=", envir = other, inherits = FALSE)) {
      next
    }

    other_bind <- other[[":="]]
    if (is.function(other_bind) && !identical(other_bind, bind)) {
      return(TRUE)
    }
  }

  FALSE
}

# conflictRules() has been in base since R 3.6.0, so it is always available
# given our R >= 4.2.0 requirement.
activate_bind_compatibility <- function() {
  for (package in c("data.table", "rlang")) {
    rule <- conflictRules(package)
    conflictRules(
      package,
      mask.ok = rule$mask.ok,
      exclude = union(rule$exclude, ":=")
    )
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
