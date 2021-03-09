text <- class_new("text", parent = "character", constructor = function(text = character()) object_new(.data = text))
number <- class_new("number", parent = "numeric", constructor = function(x) object_new(.data = x))

range <- class_new("range",
  constructor = function(start, end) {
    object_new(start = start, end = end)
  },
  validator = function(x) {
    if (x@end < x@start) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = list(
    start = "numeric",
    end = "numeric",
    property_new(
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
