#' Define a class union
#'
#' @description
#' A class union represents a list of possible classes. It can be used in two
#' places:
#'
#' * To allow a property to be one of a set of classes,
#'   `new_property(new_union("integer", Range))`.
#'
#' * As a convenient short-hand to define methods for multiple classes.
#'   `method(foo, new_union(X, Y)) <- f` is short-hand for
#'   `method(foo, X) <- f; method(foo, Y) <- foo`
#'
#' S7 includes built-in unions for "numeric" (integer and double vectors),
#' "atomic" (logical, numeric, character, and raw vectors) and
#' "vector" (atomic vectors, lists, and expressions).
#'
#' @param ... The classes to include in the union. See [as_class()] for
#'   details.
#' @export
#' @examples
#' logical_or_character <- new_union(class_logical, class_character)
#' logical_or_character
#'
#' Foo <- new_class("Foo", properties = list(x = logical_or_character))
#' Foo(x = TRUE)
#' Foo(x = letters[1:5])
#' try(Foo(1:3))
#'
#' bar <- new_generic("bar", "x")
#' # Use built-in union
#' method(bar, class_atomic) <- function(x) "Hi!"
#' bar
#' bar(TRUE)
#' bar(letters)
#' try(bar(NULL))
new_union <- function(...) {
  classes <- class_flatten(list(...))
  out <- list(classes = classes)
  class(out) <- "S7_union"
  out
}

is_union <- function(x) inherits(x, "S7_union")

#' @export
print.S7_union <- function(x, ...) {
  cat("<S7_union>: ", class_desc(x), "\n", sep = "")
  invisible(x)
}

#' @export
str.S7_union <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object)
}

class_flatten <- function(x) {
  x <- lapply(x, as_class)

  # Flatten unions
  is_union <- vlapply(x, is_union)
  x[!is_union] <- lapply(x[!is_union], list)
  x[is_union] <- lapply(x[is_union], function(x) x$classes)

  unique(unlist(x, recursive = FALSE, use.names = FALSE))
}

# See .onLoad() for definition
base_unions <- list()
