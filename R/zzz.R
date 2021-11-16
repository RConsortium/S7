#' An R7 object
#' @export
R7_object <- new_class(
  name = "R7_object",
  parent = NULL,
  constructor = function() {
     out <- .Call(R7_object_)
     class(out) <- "R7_object"
     out
  }
)

new_base_class <- function(name) {
  R7_class(name = name, constructor = function(.data) new_object(.data))
}

base_types <- setNames(, c("logical", "integer", "double", "numeric", "complex", "character", "factor", "raw", "function", "list", "data.frame"))

base_classes <- lapply(base_types, new_base_class)
base_classes[["NULL"]] <- new_base_class("NULL")

base_constructors <- lapply(base_types, get)


R7_generic <- new_class(
  name = "R7_generic",
  properties = list(name = "character", methods = "environment", signature = new_property(name = "signature", getter = function(x) formals(x@.data))),
  parent = "function",
  constructor = function(name, signature, fun) {
    new_object(name = name, signature = signature, methods = new.env(parent = emptyenv(), hash = TRUE), .data = fun)
  }
)

R7_method <- new_class(
  name = "R7_method",
  properties = list(generic = "R7_generic", signature = "list", fun = "function"),
  parent = "function",
  constructor = function(generic, signature, fun) {
    if (is.character(signature)) {
      signature <- list(signature)
    }
    new_object(generic = generic, signature = signature, .data = fun)
  }
)

R7_union <- new_class(
  name = "R7_union",
  properties = list(classes = "list"),
  validator = function(x) {
    for (val in x@classes) {
      if (!inherits(val, "R7_class")) {
        return(sprintf("All classes in an <R7_union> must be R7 classes:\n - <%s> is not an <R7_class>", class(val)[[1]]))
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

    new_object(classes = classes)
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
new_union <- R7_union


global_variables(c("name", "parent", "properties", "constructor", "validator"))

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
}
