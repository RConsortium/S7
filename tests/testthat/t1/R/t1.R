#' @export
another_s7_generic <- S7::new_generic("another_s7_generic", "x")

#' @export
another_s3_generic <- function(x) UseMethod("another_s3_generic")

#' @export
`Another S7 Class` <- S7::new_class("Another S7 Class", package = "t1")

.onLoad <- function(...) {
  S7::S7_on_load()
}

S7::S7_on_build()
