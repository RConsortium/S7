r7_class <- function(name, parent = r7_object, constructor = function(...) object_new(...), validator = function(x) NULL, properties = list()) {
  if (is.character(parent)) {
    parent <- class_get(parent)
  }
  obj <- constructor
  attr(obj, "name") <- name
  attr(obj, "parent") <- parent
  attr(obj, "properties") <- as_properties(properties)
  attr(obj, "constructor") <- constructor
  attr(obj, "validator") <- validator
  class(obj) <- c("r7_class", "r7_object")

  global_variables(names(properties))
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
  environment(constructor) <- topenv(environment(constructor))

  r7_class(name = name, parent = parent, constructor = constructor, validator = validator, properties = properties)
}

#' Retrive all of the class names for a class
#'
#' @param obj The R7 object to query
#' @return A character vector of all the class names for a given R7 class.
#' @export
class_names <- function(obj) {
  parent <- obj
  classes <- character()
  while(!is.null(parent)) {
    if (inherits(parent, "class_union")) {
      for (class in parent@classes) {
        classes <- c(classes, class_names(class))
      }
    } else if (inherits(parent, "r7_class")) {
      classes <- c(classes, parent@name)
    } else {
      classes <- c(classes, parent)
    }
    parent <- property_safely(parent, "parent")
  }
  unique(classes, fromLast = TRUE)
}
#class_names <- function(obj) {
  #.Call(class_names_, obj)
#}

#' Retrieve the r7 class corresponding to a name
#'
#' @param name The name of the r7 class
#' @param envir The environment to look for the name
#' @export
class_get <- function(name, envir = parent.frame()) {
  if (length(name) == 0) {
    return()
  }
  class <- get0(name, envir = envir)
  if (inherits(class, "r7_class")) {
    return(class)
  }

  # TODO: What do we do about existing S3 / S4 classes?

  # otherwise assume this is a base class, so use get_base_class
  get_base_class(name)
}

get_base_class <- function(name) {
  switch(name,
    "logical" = class_new("logical", constructor = function() logical()),
    "integer" = class_new("integer", constructor = function() integer()),
    "double" = ,
    "numeric" = class_new("numeric", constructor = function() numeric()),
    "complex" = class_new("complex", constructor = function() complex()),
    "character" = class_new("character", constructor = function() character()),
    "raw" = class_new("raw", constructor = function() raw()),
    "list" = class_new("list", constructor = function() list()),
    "closure" = ,
    "function" = class_new("function", constructor = function() function() NULL),
    "NULL" = class_new("NULL", constructor = function() NULL),
    stop(sprintf("invalid class '%s'", name), call. = FALSE)
  )
}
