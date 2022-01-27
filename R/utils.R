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
`%||%` <- function(x, y) if (length(x) == 0) y else x

collapse <- function(x, by) {
  paste(x, collapse = by)
}

method_signature <- function(generic, signature) {
  single <- length(generic@dispatch_args) == 1
  if (single) {
    signature <- class_deparse(signature[[1]])
  } else {
    classes <- vcapply(signature, class_deparse)
    signature <- paste0("list(", paste0(classes, collapse = ", "), ")")
  }

  sprintf("method(%s, %s)", generic@name, signature)
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

make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)

  as.function.default(c(args, body), envir = env)
}
