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

#' Define a new R7 class
#' @param name The name of the class
#' @param parent The parent class
#' @param constructor The constructor function
#' @param validator The validation function
#' @param properties A list of properties for the class
#' @export
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
    "character" = class_new("character", constructor = function() character()),
    "numeric" = class_new("numeric", constructor = function() numeric()),
    "function" = class_new("function", constructor = function() function() NULL),
    stop(sprintf("invalid class '%s'", name), call. = FALSE)
  )
}
