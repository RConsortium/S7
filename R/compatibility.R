# Where needed, attach an environment containing @ that works with S7
activate_backward_compatiblility <- function() {
  if (getRversion() < "4.3.0" && !"S7_at" %in% search()) {
    args <- list(list("@" = `@`), name = "S7_at", warn.conflicts = FALSE)
    do.call("attach", args)
  }
  invisible()
}

activate_attach_compatibility <- function(pkgname) {
  if (getRversion() >= "4.3.0" && !search_has_bind_conflict(pkgname)) {
    return(invisible())
  }

  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
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

activate_bind_compatibility <- function() {
  conflictRules <- get0("conflictRules", envir = baseenv(), inherits = FALSE)
  if (is.null(conflictRules)) {
    return(invisible())
  }

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
`@` <- function(object, name) {
  if (inherits(object, "S7_object")) {
    name <- as.character(substitute(name))
    prop(object, name)
  } else {
    name <- substitute(name)
    do.call(base::`@`, list(object, name))
  }
}
