is_union <- function(x) inherits(x, "R7_union")

#' Base R7 class
#'
#' @keywords internal
#' @export
R7_object <- new_class(
  name = "R7_object",
  parent = NULL,
  constructor = function() {
    .Call(R7_object_)
  }
)
check_R7 <- function(x, arg = deparse(substitute(x))) {
  if (!inherits(x, "R7_object")) {
    stop(sprintf("`%s` is not an <R7_object>", arg), call. = FALSE)
  }
}


new_base_class <- function(name) {
  default <- switch(name,
    "function" = function() {},
    getExportedValue("base", name)()
  )

  is.type <- getExportedValue("base", paste0("is.", name))

  new_class(
    name = name,
    constructor = function(.data) {
      if (missing(.data)) {
        .data <- default
      }
      new_object(.data)
    },
    validator = function(object) {
      if (!is.type(object)) {
        sprintf("Underlying data must be <%s> not %s", name, obj_desc(unclass(object)))
      }
    }
  )
}

# Define simple base types with constructors.
base_types <- setNames(, c(
  "logical", "integer", "double", "complex", "character", "raw",
  "list", "expression",
  "function", "environment"
))
base_classes <- lapply(base_types, new_base_class)
base_constructors <- lapply(base_types, get)

# See .onLoad() for definition
base_unions <- list()

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

R7_method <- new_class("R7_method",
  parent = "function",
  properties = list(
    generic = R7_generic,
    signature = "list"
  )
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

#' Define a class union
#'
#' @description
#' A class union represents a list of possible classes. It can be used in two
#' places:
#'
#' * To allow a property to be one of a set of classes,
#'   `new_property("x", new_union("integer", Range))`.
#'
#' * As a convenient short-hand to define methods for multiple classes.
#'   `method(foo, new_union(X, Y)) <- f` is short-hand for
#'   `method(foo, X) <- f; method(foo, Y) <- foo`
#'
#' R7 includes built-in unions for "numeric" (integer and double vectors),
#' "atomic" (logical, numeric, character, and raw vectors) and
#' "vector" (atomic vectors, lists, and expressions).
#'
#' @param ... The classes to include in the union. See [as_class()] for
#'   permitted definitions
#' @export
#' @examples
#' logical_or_character <- new_union("logical", "character")
#' logical_or_character
#'
#' Foo <- new_class("Foo", properties = list(x = logical_or_character))
#' Foo(x = TRUE)
#' Foo(x = letters[1:5])
#' try(Foo(1:3))
#'
#' bar <- new_generic("bar", "x")
#' # Use built-in union
#' method(bar, "atomic") <- function(x) "Hi!"
#' bar
#' bar(TRUE)
#' bar(letters)
#' try(bar(NULL))
new_union <- R7_union

global_variables(c("name", "parent", "properties", "constructor", "validator"))

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
}

.onLoad <- function(...) {
  base_unions$numeric <<- new_union("integer", "double")
  base_unions$atomic <<- new_union("logical", "integer", "double", "complex", "character", "raw")
  base_unions$vector <<- new_union("logical", "integer", "double", "complex", "character", "raw", "expression", "list")
}
