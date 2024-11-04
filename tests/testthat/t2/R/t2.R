
#' @export
an_s7_class <- S7::new_class("an_s7_class")

#' @importFrom t0 an_s7_generic
S7::method(an_s7_generic, S7::class_character) <- function(x) "foo"
S7::method(an_s7_generic, an_s7_class) <- function(x) "foo"

#' @importFrom t0 an_s3_generic
S7::method(an_s3_generic, an_s7_class) <- function(x) "foo"


#' @importFrom t0 AnS7Class
#' @export
AnS7Class2 <- S7::new_class("AnS7Class2", properties = list(bar = AnS7Class))

AnInternalClass <- S7::new_class("AnInternalClass", properties = list(
  foo = AnS7Class,
  bar = AnS7Class2
))


another_s7_generic <- S7::new_external_generic("t1", "another_s7_generic", "x")
S7::method(another_s7_generic, S7::class_character) <- function(x) "foo"
S7::method(another_s7_generic, an_s7_class) <- function(x) "foo"

another_s3_generic <- S7::new_external_generic("t1", "another_s3_generic", "x")
S7::method(another_s3_generic, an_s7_class) <- function(x) "foo"


.onLoad <- function(libname, pkgname) {
  S7::methods_register()
}
