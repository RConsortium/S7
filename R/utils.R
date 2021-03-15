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
