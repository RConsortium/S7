

`%||%` <- function(x, y) if (is.null(x)) y else x

new_function <- function(args = NULL,
                         body = NULL,
                         env = asNamespace("S7")) {
  as.function.default(c(args, body) %||% list(NULL), env)
}
