

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


dbg <- function(..., .display = utils::str) {
  out <- NULL
  exprs <- as.list(substitute(list(...)))[-1L]

  for (i in seq_len(...length())) {
    ..i <- as.symbol(sprintf("..%i", i))
    if (eval(substitute(missing(..i)))) next

    name <- names(exprs)[[i]]
    expr <- deparse1(exprs[[i]])

    label <- if (is.null(name)) {
      sprintf("`%s`", expr)
    } else {
      sprintf("(%s) `%s`", name, expr)
    }
    cat(label, ": ", sep = "")
    .display(out <- eval(..i))
  }

  cl <- sys.call()
  filepath <- utils::getSrcFilename(cl)

  if (length(filepath)) {
    if (!file.exists(filepath) && file.exists(file.path("R", filepath))) {
      filepath <- file.path("R", filepath)
    }

    lineno <- utils::getSrcLocation(cl)

    if (isNamespaceLoaded("cli")) {
      cli <- asNamespace("cli")
      loc <- cli$col_grey(cli$style_hyperlink(
        sprintf("(from %s:%i)", filepath, lineno),
        sprintf("file://%s", normalizePath(filepath, mustWork = FALSE)),
        params = c(line = lineno)
      ))
    } else {
      loc <- sprintf("(from %s:%i)", filepath, lineno)
    }

    cat(loc, "\n")
  } else {
    cat(sprintf("(from call: %s (srcfile missing))\n",
                trimws(deparse1(sys.call(-2), width.cutoff = 60))))
  }

  invisible(out)
}
