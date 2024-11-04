#' Register an S7 class with S4
#'
#' If you want to use [method<-] to register an method for an S4 generic with
#' an S7 class, you need to call `S4_register()` once.
#'
#' @param class An S7 class created with [new_class()].
#' @param env Expert use only. Environment where S4 class will be registered.
#' @returns Nothing; the function is called for its side-effect.
#' @export
#' @examples
#' methods::setGeneric("S4_generic", function(x) {
#'   standardGeneric("S4_generic")
#' })
#'
#' Foo <- new_class("Foo")
#' S4_register(Foo)
#' method(S4_generic, Foo) <- function(x) "Hello"
#'
#' S4_generic(Foo())
S4_register <- function(class, env = parent.frame()) {
  if (!is_class(class)) {
    msg <- sprintf("`class` must be an S7 class, not a %s", obj_desc(class))
  }

  methods::setOldClass(class_dispatch(class), where = topenv(env))
  invisible()
}

is_S4_class <- function(x) inherits(x, "classRepresentation")

S4_to_S7_class <- function(x, error_base = "") {
  # Silence R CMD check false positives
  distance <- subClass <- className <- package <- NULL

  # Convert generator function to class
  if (methods::is(x, "classGeneratorFunction")) {
    return(S4_to_S7_class(methods::getClass(as.character(x@className)), error_base))
  }

  if (methods::is(x, "ClassUnionRepresentation")) {
    subclasses <- Filter(function(y) y@distance == 1, x@subclasses)
    subclasses <- lapply(subclasses, function(x) methods::getClass(x@subClass))
    do.call("new_union", subclasses)
  } else if (methods::is(x, "classRepresentation")) {
    if (x@package == "methods") {
      basic_classes <- S4_basic_classes()
      if (hasName(basic_classes, x@className)) {
        return(basic_classes[[x@className]])
      }
    }
    if (methods::extends(x, "oldClass")) {
      new_S3_class(as.character(x@className))
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

S4_basic_classes <- function() {
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
    environment = class_environment,
    name = class_name,
    call = class_call,
    data.frame = class_data.frame,
    Date = class_Date,
    factor = class_factor,
    POSIXct = class_POSIXct,
    POSIXlt = class_POSIXlt,
    POSIXt = class_POSIXt,
    # matrix = class_matrix,
    # array = class_array,
    formula = class_formula
  )
}

S4_class_dispatch <- function(x) {
  x <- methods::getClass(x)
  self <- S4_class_name(x)

  # Find class objects for super classes
  extends <- unname(methods::extends(x, fullInfo = TRUE))
  extends <- Filter(function(x) methods::is(x, "SClassExtension"), extends)
  classes <- lapply(extends, function(x) methods::getClass(x@superClass))

  # Remove unions: S7 handles them in method registration, not dispatch.
  classes <- Filter(function(x) !methods::is(x, "ClassUnionRepresentation"), classes)
  # Remove specially named union base classes
  classes <- Filter(function(x) !x@className %in% c("oldClass", "vector"), classes)

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

  if (identical(package, "methods") && class %in% names(S4_basic_classes())) {
    class
  } else if (is.null(package) || identical(package, ".GlobalEnv")) {
    paste0("S4/", class)
  } else {
    paste0("S4/", package, "::", class)
  }
}

S4_remove_classes <- function(classes, where = parent.frame()) {
  for (class in classes) {
    suppressWarnings(methods::removeClass(class, topenv(where)))
  }
}

globalVariables(c("superClass", "virtual"))
