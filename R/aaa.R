

`%||%` <- function(x, y) if (is.null(x)) y else x

new_function <- function(args = NULL,
                         body = NULL,
                         env = asNamespace("S7")) {
  as.function.default(c(args, body) %||% list(NULL), env)
}


`append<-` <- function(x, after, value) {
  if (missing(after))
    c(x, value)
  else
    append(x, value, after = after)
}

`append1<-` <- function (x, value) {
  stopifnot(is.list(x) || identical(mode(x), mode(value)))
  x[[length(x) + 1L]] <- value
  x
}


topNamespaceName <- function(env = parent.frame()) {
  env <- topenv(env)
  if (!isNamespace(env)) {
    return() # print visible
  }

  as.character(getNamespaceName(env)) # unname
}

is_string <- function(x) {
  identical(class(x), "character") && length(x) == 1L && !is.na(x) && x != ""
}
