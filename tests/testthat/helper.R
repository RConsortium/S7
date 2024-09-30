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
