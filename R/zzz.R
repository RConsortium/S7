#' An R7 object
#' @export
R7_object <- NULL

#' R7 generics and method objects
#' @param name,generic The name or generic object of the generic
#' @param signature The signature of the generic
#' @param fun The function to use as the body of the generic.
#' @export
R7_generic <- NULL

#' @rdname R7_generic
#' @export
R7_method <- NULL

#' Class unions
#'
#' A class union represents a list of possible classes. It is used in
#' properties to allow a property to be one of a set of classes, and in method
#' dispatch as a convenience for defining a method for multiple classes.
#' @param ... The classes to include in the union, either looked up by named or
#'   by passing the `R7_class` objects directly.
#' @export
R7_union <- NULL

#' @rdname R7_union
#' @export
new_union <- NULL

#' R7 base classes
#' @param x default value for the object.
#' @rdname R7_base
#' @export
lgl <- NULL

#' @rdname R7_base
#' @export
int <- NULL

#' @rdname R7_base
#' @export
dbl <- NULL

#' @rdname R7_base
#' @export
num <- NULL

#' @rdname R7_base
#' @export
cpl <- NULL

#' @rdname R7_base
#' @export
chr <- NULL

#' @rdname R7_base
#' @export
fct <- NULL

#' @rdname R7_base
#' @export
raw <- NULL

#' @rdname R7_base
#' @export
fn <- NULL

#' @rdname R7_base
#' @export
lst <- NULL

#' @rdname R7_base
#' @export
df <- NULL

#' @rdname R7_base
#' @export
env <- NULL

.onLoad <- function(...) {
  source(system.file("R7", "onload.R", package = "R7"), local = TRUE)
}

global_variables(c("name", "parent", "properties", "constructor", "validator"))

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
}
