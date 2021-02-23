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

#' @export
print.r7_object <- function(x, ...) {
  cat(sprintf("r7: <%s>\n", object_class(x)@name))
}


get_base_class <- function(name) {
  switch(name,
    "logical" = class_new("logical", constructor = function() logical()),
    "integer" = class_new("integer", constructor = function() integer()),
    "double" = ,
    "numeric" = class_new("numeric", constructor = function() numeric()),
    "complex" = class_new("numeric", constructor = function() complex()),
    "character" = class_new("character", constructor = function() character()),
    "raw" = class_new("raw", constructor = function() raw()),
    "list" = class_new("raw", constructor = function() list()),
    "closure" = ,
    "function" = class_new("function", constructor = function() function() NULL),
    stop(sprintf("invalid class '%s'", name), call. = FALSE)
  )
}
