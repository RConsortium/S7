#' Define a class union
#'
#' @description
#' A class union represents a list of possible classes. You can create it
#' with `new_union(a, b, c)` or `a | b | c`. Unions can be used in two
#' places:
#'
#' * To allow a property to be one of a set of classes,
#'   `new_property(class_integer | Range)`. The default `default` value for the
#'   property will be the constructor of the first object in the union.
#'   This means if you want to create an "optional" property (i.e. one that
#'   can be `NULL` or of a specified type), you'll need to write (e.g.)
#'   `NULL | class_integer`.
#'
#' * As a convenient short-hand to define methods for multiple classes.
#'   `method(foo, X | Y) <- f` is short-hand for
#'   `method(foo, X) <- f; method(foo, Y) <- foo`
#'
#' S7 includes built-in unions for "numeric" (integer and double vectors),
#' "atomic" (logical, numeric, complex, character, and raw vectors) and
#' "vector" (atomic vectors, lists, and expressions).
#'
#' @param ... The classes to include in the union. See [as_class()] for
#'   details.
#' @return An S7 union, i.e. a list with class `S7_union`.
#' @export
#' @examples
#' logical_or_character <- new_union(class_logical, class_character)
#' logical_or_character
#' # or with shortcut syntax
#' logical_or_character <- class_logical | class_character
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

#' @export
`|.S7_class` <- function(e1, e2) {
  new_union(e1, e2)
}
# Register remaining methods onLoad so that their pointers are identical,
# working around a bug that was fixed in R 4.1:
# https://github.com/wch/r-source/commit/b41344e3d0da7d78fd
on_load_define_or_methods <- function() {
  registerS3method("|", "S7_union", `|.S7_class`)
  registerS3method("|", "S7_base_class", `|.S7_class`)
  registerS3method("|", "S7_S3_class", `|.S7_class`)
  registerS3method("|", "S7_any", `|.S7_class`)
  registerS3method("|", "S7_missing", `|.S7_class`)
  registerS3method("|", "classGeneratorFunction", `|.S7_class`)
  registerS3method("|", "ClassUnionRepresentation", `|.S7_class`)
  registerS3method("|", "classRepresentation", `|.S7_class`)
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
