new_base_class <- function(name) {
  default <- switch(name,
    "function" = function() {},
    getExportedValue("base", name)()
  )
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

# Define simple base types with constructors.
base_types <- setNames(, c(
  "logical", "integer", "double", "complex", "character", "raw",
  "list", "expression",
  "function", "environment"
))
base_classes <- lapply(base_types, new_base_class)
base_constructors <- lapply(base_types, get)
