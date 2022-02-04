is_union <- function(x) inherits(x, "R7_union")

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

# Define simple base types with constructors. See .onLoad() for more
base_types <- setNames(, c(
  "logical", "integer", "double", "complex", "character", "raw",
  "list", "expression",
  "function", "environment"
))
base_classes <- lapply(base_types, new_base_class)
base_constructors <- lapply(base_types, get)

R7_generic <- new_class(
  name = "R7_generic",
  properties = list(
    name = "character",
    methods = "environment",
    dispatch_args = new_property(
      name = "dispatch_args",
      getter = function(x) formals(r7_data(x))
  )),
  parent = "function",
  constructor = function(name, dispatch_args, fun) {
    new_object(
      name = name,
      dispatch_args = dispatch_args,
      methods = new.env(parent = emptyenv(), hash = TRUE),
      .data = fun
    )
  }
)

R7_method <- new_class(
  name = "R7_method",
  properties = list(generic = R7_generic, signature = "list", fun = "function"),
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
  properties = list(
    new_property(
      "classes",
      setter = function(x, val) {
        x@classes <- class_flatten(val)
        x
      }
    )
  ),
  constructor = function(...) {
    new_object(classes = list(...))
  }
)

#' @export
str.R7_union <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  cat("<R7_union>: ", class_desc(object), sep = "")
  cat("\n")

  if (nest.lev == 0) {
    props <- props(object)
    props$srcref <- NULL
    str_list(props, ..., prefix = "@", nest.lev = nest.lev)
  }
}


class_flatten <- function(x) {
  x <- lapply(x, as_class)

  # Flatten unions
  is_union <- vlapply(x, is_union)
  x[!is_union] <- lapply(x[!is_union], list)
  x[is_union] <- lapply(x[is_union], function(x) x@classes)

  unique(unlist(x, recursive = FALSE, use.names = FALSE))
}

#' @export
print.R7_union <- function(x, ...) {
  cat(sprintf("<R7_union>: %s", class_desc(x)), "\n", sep = "")
  invisible(x)
}

#' Class unions
#'
#' A class union represents a list of possible classes. It is used in
#' properties to allow a property to be one of a set of classes, and in method
#' dispatch as a convenience for defining a method for multiple classes.
#'
#' @param ... The classes to include in the union, either looked up by named or
#'   by passing the `R7_class` objects directly.
#' @export
new_union <- R7_union


global_variables(c("name", "parent", "properties", "constructor", "validator"))

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
}

.onLoad <- function(...) {
  base_classes$`NULL` <- new_base_class("NULL")
  base_classes$numeric <<- new_union("integer", "double")
  base_classes$atomic <<- new_union("logical", "integer", "double", "complex", "character", "raw")
  base_classes$vector <<- new_union("logical", "integer", "double", "complex", "character", "raw", "expression", "list")
}
