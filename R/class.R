R7_class <- function(name, parent = R7_object, constructor = function(.data = NULL, ...) new_object(.data, ...), validator = function(x) NULL, properties = list()) {
  if (is.character(parent)) {
    parent_obj <- class_get(parent)
    if (!is.null(parent_obj) && inherits(parent_obj, "R7_class")) {
      parent <- parent_obj
    }
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
new_class <- function(name, parent = R7_object, constructor = function(.data = NULL, ...) new_object(.data, ...), validator = function(x) NULL, properties = list()) {
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
      classes <- c(classes, parent@name, "R7_object")
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
  if (length(name) != 1) {
    return()
  }
  class <- get0(name, envir = envir)
  if (inherits(class, "R7_class")) {
    return(class)
  }

  # TODO: What do we do about existing S3 / S4 classes?

  # otherwise assume this is a base class, so use get_base_class
  base_classes[[name]]
}

#' @export
print.R7_class <- function(x, ...) {
  props <- properties(x)
  if (length(props) > 0) {
    prop_names <- format(names(props))
    prop_types <- format(paste0("<", vcapply(props, function(xx) xx[["class"]][[1]] %||% ""), ">"), justify = "right")
    prop_fmt <- paste0(paste0(" $", prop_names, " ", prop_types, collapse = "\n"), "\n")
  } else {
    prop_fmt <- ""
  }
  parent <- property_safely(x, "parent")
  parent <- property_safely(parent, "name") %||% parent %||% ""

  cat(sprintf("<R7_class>\n@name %s\n@parent <%s>\n@properties\n%s", x@name, parent, prop_fmt), sep = "")
}
