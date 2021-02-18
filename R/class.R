r7_class <- function(name, parent = r7_object, constructor = function(...) object_new(...), validator = function(x) NULL, properties = list()) {
  if (is.character(parent)) {
    parent <- get_base_class(parent)
  }
  obj <- constructor
  attr(obj, "name") <- name
  attr(obj, "parent") <- parent
  attr(obj, "properties") <- properties
  attr(obj, "constructor") <- constructor
  attr(obj, "validator") <- validator
  class(obj) <- c("r7_class", "r7_object")
  obj
}

class_new <- function(name, parent = r7_object, constructor = function(...) object_new(...), validator = function(x) NULL, properties = list()) {
  r7_class(name = name, parent = parent, constructor = constructor, validator = validator, properties = properties)
}

validate <- function(obj) {
  obj_class <- object_class(obj)
  validator <- prop(obj_class, "validator")

  errors <- validator(obj)

  if (length(errors) > 0) {
    msg <- sprintf("invalid '%s' object:\n%s", prop(obj_class, "name"), paste0("- ", errors, collapse = "\n"))
    stop(msg, call. = FALSE)
  }

  invisible(obj)
}


#' @export
print.r7_object <- function(x, ...) {
  cat(sprintf("r7: <%s>\n", object_class(x)@name))
}


get_base_class <- function(name) {
  switch(name,
    character = class_new("character", constructor = function() character()),
    stop("invalid class 'name'", call. = FALSE)
  )
}
