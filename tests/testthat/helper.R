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

# Simulate a package with namespace
local_package <- function(
  name,
  code = {},
  version = "0.0.0",
  frame = parent.frame()
) {
  ns <- new.env(parent = asNamespace("S7"))

  info <- new.env(parent = emptyenv())
  info$spec <- c(name = name, version = version)
  info$exports <- new.env(parent = emptyenv())
  ns[[".__NAMESPACE__."]] <- info
  ns[[".packageName"]] <- name
  ns[[".__S3MethodsTable__."]] <- new.env(parent = emptyenv())

  # register namespace so asNamespace(pkg) works
  internal <- get(".Internal", envir = baseenv())
  internal(registerNamespace(name, ns))
  defer(internal(unregisterNamespace(name)), frame = frame)
  defer(S7_on_unload_(ns), frame = frame)

  eval(substitute(code), ns)

  # export everything defined by the code block so `pkg::name` works
  for (nm in ls(ns)) {
    assign(nm, nm, envir = info$exports)
  }

  ns
}

# Filter out any IDE hooks added interactively (e.g. Positron registers an
# "ark_onload_hook" for every package), leaving only the hooks S7 manages.
package_hooks <- function(package, event = "onLoad") {
  hooks <- getHook(packageEvent(package, event))
  is_ide_hook <- function(hook) any(startsWith(class(hook), "ark_"))
  Filter(function(hook) !is_ide_hook(hook), hooks)
}

local_S4_class <- function(
  name,
  ...,
  env = parent.frame(),
  where = topenv(env)
) {
  out <- methods::setClass(name, ..., where = where)
  defer(S4_remove_classes(name, where), env)
  out
}

local_S4_union <- function(name, members, env = parent.frame()) {
  out <- methods::setClassUnion(name, members, where = topenv(env))
  defer(S4_remove_classes(name, topenv(env)), env)
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

# Define an S3 method in globalenv() and remove it again on exit.
local_s3_method <- function(name, fun, frame = parent.frame()) {
  assign(name, fun, envir = globalenv())
  defer(rm(list = name, envir = globalenv()), frame = frame)
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

dbg <- function(..., .display = utils::str, .file = NULL) {
  out <- NULL
  exprs <- as.list(substitute(list(...)))[-1L]

  if (!is.null(.file)) {
    sink(.file, append = TRUE)
    defer(sink())
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
