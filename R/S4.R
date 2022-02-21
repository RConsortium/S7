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
  }

  methods::setOldClass(class_dispatch(class), where = topenv(env))
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
    } else if (x@package == "methods" && x@className %in% names(base_classes)) {
      # Convert S4 representation of base types to R7 representation
      base_classes[[x@className]]
    } else if (x@package == "methods" && x@className == "NULL") {
      NULL
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

# R7 handles unions at method registration time, where as S4 handles them at
# dispatch time.
S4_strip_union <- function(class_names) {
  classes <- lapply(class_names, methods::getClass)
  is_union <- vlapply(classes, methods::is, "ClassUnionRepresentation")

  setdiff(class_names[!is_union], "vector")
}
