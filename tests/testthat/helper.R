quick_install <- function(package, lib, quiet = TRUE) {
  opts <- c(
    "--data-compress=none",
    "--no-byte-compile",
    "--no-data",
    "--no-demo",
    "--no-docs",
    "--no-help",
    "--no-html",
    "--no-libs",
    "--use-vanilla",
    NULL
  )

  for (p in package) {
    install.packages(
      pkgs = p,
      lib = lib,
      repos = NULL,
      type = "source",
      quiet = quiet,
      INSTALL_opts = paste(opts, collapse = " ")
    )
  }
}

quick_test <- function() {
  identical(Sys.getenv("R_TESTTHAT_QUICK", "false"), "true")
}

quick_test_disable <- function() {
  Sys.setenv("R_TESTTHAT_QUICK" = "false")
}

quick_test_enable <- function() {
  Sys.setenv("R_TESTTHAT_QUICK" = "true")
}

scrub_environment <- function(x) {
  gsub("environment: 0x[0-9a-f]+", "environment: 0x0", x)
}

local_methods <- function(..., frame = parent.frame()) {
  generics <- list(...)
  methods <- lapply(generics, function(x) as.list(x@methods))
  defer(for(i in seq_along(methods)) {
    env <- generics[[i]]@methods
    rm(list = ls(envir = env), envir = env)
    list2env(methods[[i]], envir = env)
  }, frame = frame)
  invisible()
}

local_S4_class <- function(name, ..., env = parent.frame()) {
  out <- methods::setClass(name , contains = "character")
  defer(S4_remove_classes(name, env), env)
  out
}

# Lightweight equivalent of withr::defer()
defer <- function(expr, frame = parent.frame(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = frame)
}

# always returns a named list, even in the empty case.
named_list <- function(...) {
  x <- list(...)
  names(x) <- names2(x)
  x
}

`:=` <- function(sym, val) {
  cl <- sys.call()
  cl[[1L]] <- quote(`<-`)
  stopifnot(is.symbol(cl[[2L]]) && is.call(cl[[3L]]))
  cl[[3L]]$name <- as.character(cl[[2L]])
  eval.parent(cl)
}

`add<-` <- `+`

dbg <- function(..., .display = utils::str, .file = NULL) {
  out <- NULL
  exprs <- as.list(substitute(list(...)))[-1L]

  if (!is.null(.file)) {
    sink(.file, append = TRUE)
    on.exit(sink())
  }

  for (i in seq_len(...length())) {
    ..i <- as.symbol(sprintf("..%i", i))
    if (eval(substitute(missing(..i)))) {
      next
    }

    name <- names(exprs)[[i]]
    expr <- deparse1(exprs[[i]])

    label <- if (is.null(name)) {
      sprintf("`%s`", expr)
    } else {
      sprintf("(%s) `%s`", name, expr)
    }
    cat(label, if (identical(.display, utils::str)) ": " else "\n", sep = "")
    .display(out <- eval(..i))
  }

  cl <- sys.call()
  filepath <- utils::getSrcFilename(cl)

  if (length(filepath)) {
    if (!file.exists(filepath) &&
        file.exists(file.path("R", filepath))) {
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
    cat(sprintf("(from call: %s (srcfile missing))\n", trimws(
      deparse1(sys.call(-2) %error% sys.call(-1), width.cutoff = 60)
    )))
  }

  invisible(out)
}

`%error%` <- function(x, y) tryCatch(x, error = function(e) y)

drop_attributes <- function(x) {
  attributes(x) <- NULL
  x
}
