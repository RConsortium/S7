#' @export
an_s7_class <- S7::new_class("an_s7_class")

#' @importFrom t0 an_s7_generic
S7::method(an_s7_generic, S7::class_character) <- function(x) "foo"
S7::method(an_s7_generic, an_s7_class) <- function(x) "foo"

#' @importFrom t0 an_s3_generic
S7::method(an_s3_generic, an_s7_class) <- function(x) "foo"


#' @rawNamespace importFrom(t0, `An S7 Class`)
#' @export
`An S7 Class 2` <- S7::new_class(
  "An S7 Class 2",
  properties = list(bar = `An S7 Class`)
)
NULL

`An Internal Class` <- S7::new_class(
  "An Internal Class",
  properties = list(
    foo = `An S7 Class`,
    bar = `An S7 Class 2`
  )
)


another_s7_generic <- S7::new_external_generic("t1", "another_s7_generic", "x")
S7::method(another_s7_generic, S7::class_character) <- function(x) "foo"
S7::method(another_s7_generic, an_s7_class) <- function(x) "foo"

another_s3_generic <- S7::new_external_generic("t1", "another_s3_generic", "x")
S7::method(another_s3_generic, an_s7_class) <- function(x) "foo"

# A local generic with a method dispatched on a class from a soft dep (t1).
# Must register lazily, after t1 is loaded.
#' @export
own_generic <- S7::new_generic("own_generic", "x")
S7::method(own_generic, S7::new_external_class("t1", "Another S7 Class")) <-
  function(x) "from t1 class"


.onLoad <- function(libname, pkgname) {
  S7::S7_on_load()
}
