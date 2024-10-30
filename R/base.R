new_base_class <- function(name, constructor_name = name) {
  force(name)

  constructor <- new_function(
    args = list(.data = base_default(name)),
    body = quote(.data),
    env = baseenv()
  )

  validator <- function(object) {
    if (base_class(object) != name) {
      sprintf("Underlying data must be <%s> not <%s>",
              name, base_class(object))
    }
  }

  validator <- utils::removeSource(validator)

  out <- list(
    class = name,
    constructor_name = constructor_name,
    constructor = constructor,
    validator = validator
  )
  class(out) <- "S7_base_class"
  out
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(nameOfClass,S7_base_class)
nameOfClass.S7_base_class <- function(x) {
  x[["class"]]
}

base_default <- function(type) {
  switch(type,
    logical = logical(),
    integer = integer(),
    double = double(),
    complex = complex(),
    character = character(),
    raw = raw(),
    list = list(),
    expression = expression(),
    name = quote(quote(x)),
    call = quote(quote({})),

    `function` = quote(function() NULL),
    environment = quote(new.env(parent = emptyenv()))
)}


is_base_class <- function(x) inherits(x, "S7_base_class")

#' @export
print.S7_base_class <- function(x, ...) {
  cat("<S7_base_class>: ", class_desc(x), "\n", sep = "")
  invisible(x)
}

#' @export
str.S7_base_class <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object, ..., nest.lev = nest.lev)
}

#' S7 wrappers for base types
#'
#' @description
#' The following S7 classes represent base types allowing them to be used
#' within S7:
#'
#' * `class_logical`
#' * `class_integer`
#' * `class_double`
#' * `class_complex`
#' * `class_character`
#' * `class_raw`
#' * `class_list`
#' * `class_expression`
#' * `class_name`
#' * `class_call`
#' * `class_function`
#' * `class_environment` (can only be used for properties)
#'
#' We also include three union types to model numerics, atomics, and vectors
#' respectively:
#'
#' * `class_numeric` is a union of `class_integer` and `class_double`.
#' * `class_atomic` is a union of `class_logical`, `class_numeric`,
#'   `class_complex`, `class_character`, and `class_raw`.
#' * `class_vector` is a union of `class_atomic`, `class_list`, and
#'   `class_expression`.
#' * `class_language` is a union of `class_name` and `class_call`.
#'
#' @order 0
#' @name base_classes
#' @return S7 classes wrapping around common base types and S3 classes.
#' @examples
#'
#' class_integer
#' class_numeric
#' class_factor
NULL

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_logical <- new_base_class("logical")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_integer <- new_base_class("integer")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_double <- new_base_class("double")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_complex <- new_base_class("complex")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_character <- new_base_class("character")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_raw <- new_base_class("raw")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_list <- new_base_class("list")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_expression <- new_base_class("expression")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_name <- new_base_class("name")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_call <- new_base_class("call")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_function <- new_base_class("function", "fun")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 1
class_environment <- new_base_class("environment")

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 2
class_numeric <- NULL

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 2
class_atomic <- NULL

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 2
class_vector <- NULL

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 2
class_language <- NULL

# Define onload to avoid dependencies between files
on_load_define_union_classes <- function() {
  class_numeric <<- new_union(class_integer, class_double)
  class_atomic <<- new_union(class_logical, class_numeric, class_complex, class_character, class_raw)
  class_vector <<- new_union(class_atomic, class_expression, class_list)
  class_language <<- new_union(class_name, class_call)
}
