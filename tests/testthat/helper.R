text <- class_new("text", parent = "character", constructor = function(text = character()) object_new(.data = text))
number <- class_new("number", parent = "numeric", constructor = function(x) object_new(.data = x))

range <- class_new("range",
  constructor = function(start, end) {
    object_new(start = start, end = end)
  },
  validator = function(x) {
    if (property(x, "end") < property(x, "start")) {
      "`end` must be greater than or equal to `start`"
    }
  },
  properties = list(
    start = "numeric",
    end = "numeric",
    property_new(
      name = "length",
      class = "numeric",
      accessor = function(x) x@end - x@start
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
