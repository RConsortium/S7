new_base_class <- function(name, default) {
  constructor <- function(.data) {
    if (missing(.data)) {
      .data <- default
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

is_base_class <- function(x) inherits(x, "R7_base_class")

#' @export
print.R7_base_class <- function(x, ...) {
  cat("<R7_base_class>: ", class_desc(x), "\n", sep = "")
  invisible(x)
}

#' @export
str.R7_base_class <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object)
}

base_classes <- list(
  logical = new_base_class("logical", logical()),
  integer = new_base_class("integer", integer()),
  double = new_base_class("double", double()),
  complex = new_base_class("complex", complex()),
  character = new_base_class("character", character()),
  raw = new_base_class("raw", raw()),

  list = new_base_class("list", list()),
  expression = new_base_class("expression", expression()),

  `function` = new_base_class("function", function() {}),
  # TODO: create a new environment in the constructor
  environment = new_base_class("environment", new.env(parent = emptyenv()))
)
