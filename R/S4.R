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

# Find all non-virtual classes
S4_class_dispatch <- function(x) {
  x <- methods::getClass(x)
  self <- S4_class_name(x)

  # Find all extended classes, stripping self
  extends <- unname(methods::extends(x, fullInfo = TRUE))
  extends <- Filter(function(x) methods::is(x, "SClassExtension"), extends)

  classes <- lapply(extends, function(x) methods::getClass(x@superClass))
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

  if (identical(package, "methods") && class %in% names(base_classes)) {
    class
  } else if (is.null(package) || identical(package, ".GlobalEnv")) {
    paste0("S4/", class)
  } else {
    paste0("S4/", package, "::", class)
  }
}

# R7 handles unions at method registration time, where as S4 handles them at
# dispatch time.
S4_strip_union <- function(class_names) {
  classes <- lapply(class_names, methods::getClass)
  is_union <- vlapply(classes, methods::is, "ClassUnionRepresentation")

  setdiff(class_names[!is_union], "vector")
}

S4_remove_classes <- function(classes, where = parent.frame()) {
  for (class in classes) {
    methods::removeClass(class, topenv(where))
  }
}
