#' @examples
#' foo <- new_class("foo", properties = list(x = class_integer))
#' foo_ex <- new_dynamic_class(function() foo)
#' foo2 <- new_class("foo", parent = foo_ex)
#' foo2()
#'
#' foo <- new_class("foo", properties = list(x = class_integer))
#' foo_ex <- new_dynamic_class(function() foo)
#'
new_dynamic_class <- function(constructor_fun,
                              name = NULL,
                              properties = NULL,
                              constructor_args = NULL) {
  check_function(constructor_fun, alist())

  if (is.null(name)) {
    name <- constructor_fun()@name
  } else {
    check_name(name)
  }

  # Must be initialized before properties
  if (is.null(constructor_args)) {
    if (is.null(properties)) {
      constructor_args <- names(properties)
    } else {
      constructor_args <- class_constructor_args(constructor_fun())
    }
  } else {
    if (!is.character(constructor_args)) {
      stop("`constructor_args` must be a character vector")
    }
  }

  if (is.null(properties)) {
    properties <- class_properties(constructor_fun())
  } else {
    properties <- as_properties(properties)
  }


  out <- list(
    name = name,
    constructor_fun = constructor_fun,
    constructor_args = constructor_args,
    properties = properties
  )
  class(out) <- "S7_dynamic_class"
  out
}

is_dynamic_class <- function(x) {
  inherits(x, "S7_dynamic_class")
}
