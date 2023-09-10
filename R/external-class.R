#' Classes from other packages
#'
#' @description
#' You need an explicit external class when you want extend a class defined in
#' another package. An external class ensures that the class definition from
#' the other package is not literally inlined in your package, ensuring that
#' when the other package changes your package doesn't need to be rebuilt to
#' get those changes.
#'
#' Extending a class creates a hard requirement on the package that defines it;
#' i.e. you must list the package in the `Imports` field in your package's
#' `DESCRIPTION`.
#'
#' @param package Package the class is defined in.
#' @param name Name of class, as a string.
#' @param constructor_fun A zero-argument function that yields the constructor
#'   of the external class. For expert use only.
#' @returns An S7 external class, i.e. a list with class
#'   `S7_external_class`.
#' @export
#' @examples
#' foo <- new_class("foo", properties = list(x = class_integer))
#' foo_ex <- new_external_class("S7", "foo", function() foo)
#'
#' foo2 <- new_class("foo", parent = foo_ex)
#' foo2()
new_external_class <- function(package,
                               name,
                               constructor_fun = NULL) {

  check_name(package)
  check_name(name)

  if (is.null(constructor_fun)) {
    constructor_fun <- function() getExportedValue(package, name)
  } else {
    check_function(constructor_fun, alist())
  }

  constructor <- constructor_fun()
  if (!is_class(constructor)) {
    stop("`constructor_fun()` must yield an S7 class")
  }

  out <- list(
    package = package,
    name = name,
    constructor_fun = constructor_fun
  )
  class(out) <- "S7_external_class"
  out
}

#' @export
print.S7_external_class <- function(x, ...) {
  cat(
    "<S7_external_class> ",
    x$package, "::", x$name, "\n",
    sep = ""
  )
  invisible(x)
}


is_external_class <- function(x) {
  inherits(x, "S7_external_class")
}
