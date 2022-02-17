new_base_class <- function(name) {
  constructor <- function(.data) {
    if (missing(.data)) {
      .data <- base_default(name)
    }
    .data
  }

  is.type <- getExportedValue("base", paste0("is.", name))
  validator <- function(object) {
    if (!is.type(object)) {
      sprintf("Underlying data must be <%s> not %s", name, obj_desc(unclass(object)))
    }
  }

  structure(
    list(
      class = name,
      constructor = constructor,
      validator = validator
    ),
    class = "R7_base_class"
  )
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


is_base_class <- function(x) inherits(x, "R7_base_class")

#' @export
print.R7_base_class <- function(x, ...) {
  cat("<R7_base_class>: ", class_desc(x), "\n", sep = "")
  invisible(x)
}

#' @export
str.R7_base_class <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object, ..., nest.lev = nest.lev)
}

base_classes <- list(
  logical = new_base_class("logical"),
  integer = new_base_class("integer"),
  double = new_base_class("double"),
  complex = new_base_class("complex"),
  character = new_base_class("character"),
  raw = new_base_class("raw"),

  list = new_base_class("list"),
  expression = new_base_class("expression"),

  `function` = new_base_class("function"),
  environment = new_base_class("environment")
)
