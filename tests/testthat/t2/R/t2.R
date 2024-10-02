
#' @importFrom t0 an_s7_generic
S7::method(an_s7_generic, S7::class_character) <- function(x) "foo"

#' @importFrom t0 an_s3_generic
S7::method(an_s3_generic, S7::class_character) <- function(x) "foo"

another_s7_generic <- S7::new_external_generic("t1", "another_s7_generic", "x")
S7::method(another_s7_generic, S7::class_character) <- function(x) "foo"

another_s3_generic <- S7::new_external_generic("t1", "another_s3_generic", "x")
S7::method(another_s3_generic, S7::class_character) <- function(x) "foo"

.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
