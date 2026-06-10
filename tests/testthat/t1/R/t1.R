#' @export
another_s7_generic <- S7::new_generic("another_s7_generic", "x")

#' @export
another_s3_generic <- function(x) UseMethod("another_s3_generic")

#' @export
`Another S7 Class` <- S7::new_class("Another S7 Class", package = "t1")

#' @export
ParentClass <- S7::new_class(
  "ParentClass",
  package = "t1",
  properties = list(parent_prop = S7::class_character)
)
