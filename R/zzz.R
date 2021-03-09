#' An R7 object
#' @export
R7_object <- class_new(
  name = "R7_object",
  parent = character(),
  constructor = function() list()
)

#' R7 generics and method objects
#' @param name,generic The name or generic object of the generic
#' @param signature The signature of the generic
#' @param fun The function to use as the body of the generic.
#' @export
R7_generic <- class_new(
  name = "R7_generic",
  properties = list(name = "character", methods = "environment", signature = "list"),
  parent = "function",
  constructor = function(name, signature, fun) {
    object_new(name = name, signature = signature, methods = new.env(parent = emptyenv(), hash = TRUE), .data = fun)
  }
)

#' @rdname R7_generic
#' @export
R7_method <- class_new(
  name = "R7_method",
  properties = list(generic = "R7_generic", signature = "list", fun = "function"),
  parent = "function",
  constructor = function(generic, signature, fun) {
    if (is.character(signature)) {
      signature <- list(signature)
    }
    object_new(generic = generic, signature = signature, .data = fun)
  }
)

#' Class unions
#'
#' A class union represents a list of possible classes. It is used in
#' properties to allow a property to be one of a set of classes, and in method
#' dispatch as a convenience for defining a method for multiple classes.
#' @param ... The classes to include in the union, either looked up by named or
#'   by passing the `R7_class` objects directly.
#' @export
class_union <- class_new(
  name = "class_union",
  properties = list(classes = "list"),
  validator = function(x) {
    for (val in x@classes) {
      if (!inherits(val, "R7_class")) {
        return(sprintf("All classes in a <class_union> must be R7 classes:\n - <%s> is not an <R7_class>", class(val)[[1]]))
      }
    }
  },
  constructor = function(...) {
    classes <- list(...)
    for (i in seq_along(classes)) {
      if (is.character(classes[[i]])) {
        classes[[i]] <- class_get(classes[[i]])
      }
    }

    object_new(classes = classes)
  }
)

global_variables(c("name", "parent", "properties", "constructor", "validator"))

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
}
