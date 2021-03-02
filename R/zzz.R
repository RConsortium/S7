#' An r7 object
#' @export
r7_object <- class_new(
  name = "r7_object",
  parent = character(),
  constructor = function() list()
)

#' r7 generics and method objects
#' @param name,generic The name or generic object of the generic
#' @param signature The signature of the generic
#' @param fun The function to use as the body of the generic.
#' @export
r7_generic <- class_new(
  name = "r7_generic",
  properties = list(name = "character", methods = "environment", signature = "list"),
  parent = "function",
  constructor = function(name, signature, fun) {
    object_new(name = name, signature = signature, methods = new.env(parent = emptyenv(), hash = TRUE), .data = fun)
  }
)

#' @rdname r7_generic
#' @export
r7_method <- class_new(
  name = "r7_method",
  properties = list(generic = "r7_generic", signature = "list", fun = "function"),
  parent = "function",
  constructor = function(generic, signature, fun) {
    object_new(generic = generic, signature = signature, .data = fun)
  }
)

#' Class unions
#'
#' A class union represents a list of possible classes. It is used in
#' properties to allow a property to be one of a set of classes, and in method
#' dispatch as a convenience for defining a method for multiple classes.
#' @param ... The classes to include in the union, either looked up by named or
#'   by passing the `r7_class` objects directly.
#' @export
class_union <- class_new(
  name = "class_union",
  properties = list(classes = "list"),
  validator = function(x) {
    for (val in x@classes) {
      if (!inherits(val, "r7_class")) {
        return(sprintf("All classes in a `class_union` must be R7 classes:\n - '%s' is not an `r7_class`", class(val)[[1]]))
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

accessor <- class_new("accessor", parent = "function", constructor = function(fun) object_new(.data = fun))

global_variables(c("name", "parent", "properties", "constructor", "validator"))

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
}
