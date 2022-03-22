#' Register an R7 class with S4
#'
#' If you want to use [method<-] to register an method for an S4 generic with
#' an R7 class, you need to call `S4_register()` once.
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
  contains <- setdiff(class_dispatch(class), "ANY")[-1]

  methods::setClass(name, contains = contains, where = topenv(env))
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
