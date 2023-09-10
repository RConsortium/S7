#' @examples
#' foo <- new_class("foo", properties = list(x = class_integer))
#' foo_ex <- new_external_class(function() foo)
#' foo2 <- new_class("foo", parent = foo_ex)
#' foo2()
#'
new_external_class <- function(constructor_fun,
                              name = NULL,
                              properties = NULL) {
  check_function(constructor_fun, alist())

  if (is.null(name)) {
    name <- constructor_fun()@name
  } else {
    check_name(name)
  }

  if (is.null(properties)) {
    properties <- class_properties(constructor_fun())
  } else {
    properties <- as_properties(properties)
  }

  out <- list(
    name = name,
    constructor_fun = constructor_fun,
    properties = properties
  )
  class(out) <- "S7_external_class"
  out
}

is_external_class <- function(x) {
  inherits(x, "S7_external_class")
}
