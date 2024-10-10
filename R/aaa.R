

`%||%` <- function(x, y) if (is.null(x)) y else x

new_function <- function(args = NULL,
                         body = NULL,
                         env = asNamespace("S7")) {
  as.function.default(c(args, body) %||% list(NULL), env)
}

`append1<-` <- function (x, value) {
  stopifnot(is.list(x) || identical(mode(x), mode(value)))
  x[[length(x) + 1L]] <- value
  x
}

topNamespaceName <- function(env = parent.frame()) {
  env <- topenv(env)
  if (isNamespace(env))
    getNamespaceName(env)
  else
    NULL
}
