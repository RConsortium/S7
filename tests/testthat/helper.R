quick_install <- function(package, lib) {
  for (p in package) {
    install.packages(p, lib, repos = NULL, type = "source", quiet = FALSE,
      INSTALL_opts = paste(collapse = " ", c(
          "--data-compress=none",
          "--no-byte-compile",
          "--no-data",
          "--no-demo",
          "--no-docs",
          "--no-help",
          "--no-help",
          "--no-html",
          "--no-libs",
          "--no-lock",
          "--no-staged-install",
          "--no-test-load",
          "--use-vanilla",
          NULL)
      )
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
