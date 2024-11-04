#' @export
an_s7_generic <- S7::new_generic("an_s7_generic", "x")

#' @export
an_s3_generic <- function(x) UseMethod("an_s3_generic")

#' @export
`An S7 Class` <- S7::new_class("An S7 Class")
