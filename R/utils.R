is_named <- function (x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(FALSE)
  }
  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }
  TRUE
}

has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(rep(FALSE, length(x)))
  }
  !(is.na(nms) | nms == "")
}

global_variables <- function(names) {
  env <- topenv(parent.frame())
  if (exists(".__global__", envir = env) && bindingIsLocked(".__global__", env = env)) {
    get("unlockBinding", baseenv())(".__global__", env = env)
    on.exit(lockBinding(".__global__", env = env))
  }
  current <- get0(".__global__", envir = env, ifnotfound = character())
  current <- unique(c(current, names))
  assign(".__global__", current, envir = env)
}

vlapply <- function(X, FUN, ...) vapply(X = X, FUN = FUN, FUN.VALUE = logical(1), ...)
vcapply <- function(X, FUN, ...) vapply(X = X, FUN = FUN, FUN.VALUE = character(1), ...)
`%||%` <- function(x, y) if (length(x) == 0 || (length(x) == 1 && !nzchar(x))) y else x

fmt_classes <- function(classes, collapse = ", ") {
  paste0("<", classes, ">", collapse = collapse)
}

collapse <- function(x, by) {
  paste(x, collapse = by)
}

method_signature <- function(signature) {
  format_signature <- function(x) {
    if (inherits(x, "R7_class")) {
      x@name
    } else {
      sprintf('"%s"', x)
    }
  }
  collapse(vcapply(signature, format_signature), by = ", ")
}

as_names <- function(x, named = FALSE) {
  if (named) {
    names(x) <- x
  }
  lapply(x, as.name)
}

names2 <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep("", length(x))
  } else {
    nms
  }
}
