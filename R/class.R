R7_class <- function(name, parent = R7_object, constructor = function(...) new_object(...), validator = function(x) NULL, properties = list()) {
  if (is.character(parent)) {
    parent <- class_get(parent)
  }
  object <- constructor
  attr(object, "name") <- name
  attr(object, "parent") <- parent
  attr(object, "properties") <- as_properties(properties)
  attr(object, "constructor") <- constructor
  attr(object, "validator") <- validator
  class(object) <- c("R7_class", "R7_object")

  global_variables(names(properties))
  object
}

#' Define a new R7 class
#' @param name The name of the class
#' @param parent The parent class
#' @param constructor The constructor function
#' @param validator The validation function
#' @param properties A list of properties for the class
#' @export
new_class <- function(name, parent = R7_object, constructor = function(...) new_object(...), validator = function(x) NULL, properties = list()) {
  environment(constructor) <- topenv(environment(constructor))

  R7_class(name = name, parent = parent, constructor = constructor, validator = validator, properties = properties)
}

#' Retrive all of the class names for a class
#'
#' @param object The R7 object to query
#' @return A character vector of all the class names for a given R7 class.
#' @export
class_names <- function(object) {
  parent <- object
  classes <- character()
  while(!is.null(parent)) {
    if (inherits(parent, "R7_union")) {
      for (class in parent@classes) {
        classes <- c(classes, class_names(class))
      }
    } else if (inherits(parent, "R7_class")) {
      classes <- c(classes, parent@name)
    } else {
      classes <- c(classes, parent)
    }
    parent <- property_safely(parent, "parent")
  }
  unique(classes, fromLast = TRUE)
}

#' Retrieve the R7 class corresponding to a name
#'
#' @param name The name of the R7 class
#' @param envir The environment to look for the name
#' @export
class_get <- function(name, envir = parent.frame()) {
  if (length(name) == 0) {
    return()
  }
  class <- get0(name, envir = envir)
  if (inherits(class, "R7_class")) {
    return(class)
  }

  # TODO: What do we do about existing S3 / S4 classes?

  # otherwise assume this is a base class, so use get_base_class
  get_base_class(name)
}

get_base_class <- function(name) {
  switch(name,
    "logical" = new_class("logical", constructor = function() logical()),
    "integer" = new_class("integer", constructor = function() integer()),
    "double" = ,
    "numeric" = new_class("numeric", constructor = function() numeric()),
    "complex" = new_class("complex", constructor = function() complex()),
    "character" = new_class("character", constructor = function() character()),
    "factor" = new_class("factor", constructor = function() factor()),
    "raw" = new_class("raw", constructor = function() raw()),
    "list" = new_class("list", constructor = function() list()),
    "closure" = ,
    "function" = new_class("function", constructor = function() function() NULL),
    "NULL" = new_class("NULL", constructor = function() NULL),
    stop(sprintf("Invalid class <%s>", name), call. = FALSE)
  )
}

#' @export
print.R7_class <- function(x, ...) {
  props <- properties(x)
  if (length(props) > 0) {
    prop_names <- format(names(props))
    prop_types <- format(paste0("<", vcapply(props, function(xx) xx[["class"]] %||% ""), ">"), justify = "right")
    prop_fmt <- paste0(paste0(" $", prop_names, " ", prop_types, collapse = "\n"), "\n")
  } else {
    prop_fmt <- ""
  }
  cat(sprintf("<R7_class>\n@name %s\n@parent <%s>\n@properties\n%s", x@name, property_safely(x, "parent")@name %||% "NULL", prop_fmt), sep = "")
}
