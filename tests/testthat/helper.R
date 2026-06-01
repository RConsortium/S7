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

# Like quick_install(), but compiles C code (so it works for S7 itself).
quick_install_dev <- function(src, lib, quiet = TRUE) {
  install.packages(
    src,
    lib = lib,
    repos = NULL,
    type = "source",
    quiet = quiet,
    INSTALL_opts = paste(
      "--no-docs",
      "--no-help",
      "--no-html",
      "--no-byte-compile",
      "--no-multiarch"
    )
  )
}

# Ensure an S7 that exports S7_on_build() is available in `lib` for building
# test packages. Under R CMD check the S7 being checked is already current;
# under devtools the loaded S7 is a source tree we can install. Skips the
# calling test if neither is available.
ensure_s7_with_hook <- function(lib, old_libpaths = .libPaths()) {
  ns_file <- file.path(find.package("S7", lib.loc = old_libpaths), "NAMESPACE")
  if (any(grepl("S7_on_build", readLines(ns_file), fixed = TRUE))) {
    return(invisible())
  }

  s7_path <- find.package("S7")
  if (file.exists(file.path(s7_path, "R", "method-register.R"))) {
    quick_install_dev(s7_path, lib)
  } else {
    testthat::skip("Installed S7 does not export S7_on_build()")
  }
  invisible()
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
  defer(
    for (i in seq_along(methods)) {
      env <- generics[[i]]@methods
      rm(list = ls(envir = env), envir = env)
      list2env(methods[[i]], envir = env)
    },
    frame = frame
  )
  invisible()
}

local_S4_class <- function(name, ..., env = parent.frame()) {
  out <- methods::setClass(name, contains = "character")
  defer(S4_remove_classes(name, env), env)
  out
}

# Create a temporary library, prepend it to .libPaths(), and restore the
# library paths and delete the temporary library when `frame` exits. Returns
# the path to the temporary library.
local_libpath <- function(frame = parent.frame()) {
  lib <- tempfile()
  dir.create(lib)
  defer(unlink(lib, recursive = TRUE), frame = frame)

  old <- .libPaths()
  .libPaths(c(lib, old))
  defer(.libPaths(old), frame = frame)
  lib
}

# Install the package at `path` into `lib`, attach it, and detach (and unload)
# it when `frame` exits. The package name is taken from `basename(path)`.
local_install_and_attach <- function(path, lib, frame = parent.frame()) {
  quick_install(path, lib)
  package <- basename(path)
  library(package, character.only = TRUE)
  defer(
    try(detach(paste0("package:", package), unload = TRUE), silent = TRUE),
    frame = frame
  )
  invisible(package)
}

# Create an S3 generic in globalenv() so that `UseMethod()` can find methods
# registered by S7 (which writes to the generic's environment's methods table).
# Cleans up the generic and any registered methods on exit.
local_s3_generic <- function(name, frame = parent.frame()) {
  eval(
    bquote(.(name) <- function(x) UseMethod(.(name))),
    globalenv()
  )
  defer(
    {
      rm(list = name, envir = globalenv())
      unregister_s3_methods(globalenv(), name)
    },
    frame = frame
  )
  invisible()
}

unregister_s3_methods <- function(envir, generic) {
  tbl <- envir[[".__S3MethodsTable__."]]
  if (!is.null(tbl)) {
    methods <- ls(tbl, pattern = paste0("^", generic, "\\."))
    rm(list = methods, envir = tbl)
  }

  invisible()
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
    if (
      !file.exists(filepath) &&
        file.exists(file.path("R", filepath))
    ) {
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
    cat(sprintf(
      "(from call: %s (srcfile missing))\n",
      trimws(
        deparse1(sys.call(-2) %error% sys.call(-1), width.cutoff = 60)
      )
    ))
  }

  invisible(out)
}

`%error%` <- function(x, y) tryCatch(x, error = function(e) y)

drop_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

# Detect whether `x` is still an ALTREP compact integer sequence (either
# directly, or wrapped — R wraps a compact_intseq in an ALTREP wrapper when
# attributes are added without forcing materialisation).
# Accesses `.Internal` indirectly so R CMD check doesn't flag it.
is_altrep_preserved <- function(x) {
  internal <- get(".Internal", envir = baseenv())
  out <- utils::capture.output(internal(inspect(x)))
  any(grepl("compact|wrapper", out))
}
