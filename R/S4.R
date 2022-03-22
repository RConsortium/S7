#' Register an R7 class with S4
#'
#' @description
#' If you want to use [method<-] to register an method for an S4 generic with
#' an R7 class, you need to call `S4_register()` once. This generates a full
#' S4 class specification that:
#'
#' * Matches
#' * Uses [validate()] as the validity method.
#' * Defines formal S4 slots to match R7 properties. The slot types are
#'   matched to the R7 property types, with the exception of R7 unions,
#'   which are unchecked (due to the challenges of converting R7 unions to
#'   S4 unions).
#'
#' When registering a class that extends R7 class or specifies an R7 class for
#' a property, you must register those classes first.
#'
#' @param class An R7 class created with [new_class()].
#' @param env Expert use only. Environment where S4 class will be registered.
#' @export
S4_register <- function(class, env = parent.frame()) {
  if (!is_class(class)) {
    msg <- sprintf("`class` must be an R7 class, not a %s", obj_desc(class))
    stop(msg)
  }

  name <- class@name
  contains <- double_to_numeric(setdiff(class_dispatch(class), "ANY")[-1])

  # S4 inherits slots from parent class, so
  props <- class@properties
  if (is_class(class@parent) && class@parent@name != "R7_object") {
    parent_props <- class@parent@properties
    props <- props[setdiff(names(props), names(parent_props))]
  }
  slots <- lapply(props, function(x) R7_to_S4_class(x$class))

  methods::setClass(name, contains = contains, slots = slots, where = topenv(env))
  methods::setValidity(name, function(object) validate(object), where = topenv(env))
  methods::setOldClass(c(name, contains), S4Class = name, where = topenv(env))
}

is_S4_class <- function(x) inherits(x, "classRepresentation")

S4_to_R7_class <- function(x, error_base = "") {
  # Silence R CMD check false postives
  distance <- subClass <- className <- package <- NULL

  # Convert generator function to class
  if (methods::is(x, "classGeneratorFunction")) {
    return(S4_to_R7_class(methods::getClass(as.character(x@className)), error_base))
  }

  if (methods::is(x, "ClassUnionRepresentation")) {
    subclasses <- Filter(function(y) y@distance == 1, x@subclasses)
    subclasses <- lapply(subclasses, function(x) methods::getClass(x@subClass))
    do.call("new_union", subclasses)
  } else if (methods::is(x, "classRepresentation")) {
    if (methods::extends(x, "oldClass")) {
      new_S3_class(as.character(x@className))
    } else if (x@package == "methods") {
      base_classes <- S4_base_classes()
      if (hasName(base_classes, x@className)) {
        base_classes[[x@className]]
      } else {
        x
      }
    } else {
      x
    }
  } else {
    msg <- sprintf(
      "Unsupported S4 object: must be a class generator or a class definition, not a %s.",
      obj_desc(x)
    )
    stop(paste0(error_base, msg), call. = FALSE)
  }
}

R7_to_S4_class <- function(x) {
  switch(class_type(x),
    NULL = "NULL",
    any = "ANY",
    S4 = S4_class_name(x),
    R7 = R7_class_name(x),
    R7_base = double_to_numeric(x$class),
    R7_S3 = x$class[[1]],
    R7_union = "ANY",
    stop("Unsupported")
  )
}

# S4 uniformly uses numeric to mean double
double_to_numeric <- function(x) {
  x[x == "double"] <- "numeric"
  x
}

S4_base_classes <- function() {
  list(
    NULL = NULL,
    logical = class_logical,
    integer = class_integer,
    double = class_double,
    numeric = class_numeric,
    character = class_character,
    complex = class_complex,
    raw = class_raw,
    list = class_list,
    expression = class_expression,
    vector = class_vector,
    `function` = class_function,
    environment = class_environment
  )
}

S4_class_dispatch <- function(x) {
  x <- methods::getClass(x)
  self <- S4_class_name(x)

  # Find class objects for super classes
  extends <- unname(methods::extends(x, fullInfo = TRUE))
  extends <- Filter(function(x) methods::is(x, "SClassExtension"), extends)
  classes <- lapply(extends, function(x) methods::getClass(x@superClass))

  # Remove virtual classes that aren't S3. This removes unions because R7
  # handles them in method registration, not dispatch.
  classes <- Filter(function(x) !x@virtual || is_oldClass(x), classes)

  c(self, vcapply(classes, S4_class_name))
}

is_oldClass <- function(x) {
  x@virtual && methods::extends(x, "oldClass") && x@className != "oldClass"
}

S4_class_name <- function(x) {
  if (is_oldClass(x)) {
    return(x@className)
  }

  class <- x@className
  package <- x@package %||% attr(class, "package")

  if (identical(package, "methods") && class %in% names(S4_base_classes())) {
    class
  } else if (is.null(package) || identical(package, ".GlobalEnv")) {
    paste0("S4/", class)
  } else {
    paste0("S4/", package, "::", class)
  }
}

S4_remove_classes <- function(classes, where = globalenv()) {
  for (class in classes) {
    methods::removeClass(class, topenv(where))
  }
}

globalVariables(c("superClass", "virtual"))
