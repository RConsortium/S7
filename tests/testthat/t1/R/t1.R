#' @export
another_s7_generic <- S7::new_generic("another_s7_generic", "x")

#' @export
another_s3_generic <- function(x) UseMethod("another_s3_generic")

.onLoad <- function(...) {
  S7::S7_on_load()
}

S7::S7_on_build()
