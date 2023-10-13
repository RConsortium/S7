new_base_class <- function(name, constructor_name = name) {
  force(name)

  constructor <- function(.data = class_missing) {
    if (is_class_missing(.data)) {
      .data <- base_default(name)
    }
    .data
  }

  validator <- function(object) {
    if (base_class(object) != name) {
      sprintf("Underlying data must be <%s> not <%s>", name, base_class(object))
    }
  }

  out <- list(
    class = name,
    constructor_name = constructor_name,
    constructor = constructor,
    validator = validator
  )
  class(out) <- "S7_base_class"
  out
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

    `function` = function() {},
    environment = new.env(parent = emptyenv())
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

#' Base classes
#'
#' @description
#' These classes represent base types allowing them to be used within S7.
#' There are three categories: base types, unions types, and key S3 classes.
#'
#' Base types:
#'
#' * `class_logical`
#' * `class_integer`
#' * `class_double`
#' * `class_complex`
#' * `class_character`
#' * `class_raw`
#' * `class_list`
#' * `class_expression`
#' * `class_function`
#' * `class_environment` (can only be used for properties)
#'
#' Union types:
#'
#' * `class_numeric` is a union of `class_integer` and `class_double`.
#' * `class_atomic` is a union of `class_logical`, `class_numeric`,
#'   `class_complex`, `class_character`, and `class_raw`.
#' * `class_vector` is a union of `class_atomic`, `class_list`, and
#'   `class_expression`.
#'
#' Key S3 classes:
#'
#' * `class_data.frame`
#' * `class_Date`
#' * `class_factor`
#' * `class_POSIXct`
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

# Define onload to avoid dependencies between files
on_load_define_union_classes <- function() {
  class_numeric <<- new_union(class_integer, class_double)
  class_atomic <<- new_union(class_logical, class_numeric, class_complex, class_character, class_raw)
  class_vector <<- new_union(class_atomic, class_expression, class_list)
}
