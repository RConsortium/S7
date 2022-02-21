range <- new_class("range",
  validator = function(self) {
    if (self@end < self@start) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = list(
    start = "numeric",
    end = "numeric",
    new_property(
      name = "length",
      class = "numeric",
      getter = function(x) x@end - x@start,
      setter = function(x, value) {
        x@end <- x@start + value
        x
      }
    )
  )
)

quick_install <- function(package) {
  for (p in package) {
    install.packages(p, repos = NULL, type = "source", quiet = TRUE,
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
