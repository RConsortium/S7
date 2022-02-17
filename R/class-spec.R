#' Standard class specifications
#'
#' This is used as the interface between R7 and R's other OO systems, allowing
#' you to use R7 classes and methods with base types, informal S3 classes, and
#' formal S4 classes.
#'
#' @param x A class specification. One of the following:
#'   * An R7 class (created by [new_class()]).
#'   * An R7 union (created by [new_union()]).
#'   * An S3 class (created by [new_S3_class()]).
#'   * An S4 class (created by [methods::getClass()] or [methods::new()]).
#'   * A base type specified either with its constructor (`logical`, `integer`,
#'     `double` etc) or its name (`"logical"`, `"integer"`, "`double`" etc).
#'   * A base union type specified by its name: `"numeric"`, `"atomic"`, or
#'      `"vector"`.
#' @param arg Argument name used when generating errors.
#' @export
#' @return A standardised class: either `NULL`, an R7 class, an R7 union,
#'   as [new_S3_class], or a S4 class.
as_class <- function(x, arg = deparse(substitute(x))) {
  error_base <- sprintf("Can't convert `%s` to a valid class", arg)

  if (is.null(x)) {
    x
  } else if (is_class(x) || is_base_class(x) || is_S3_class(x) || is_union(x)) {
    x
  } else if (isS4(x)) {
    as_S4_class(x, error_base)
  } else if (is.function(x)) {
    candidate <- find_base_name(x, names(base_classes))
    if (is.na(candidate)) {
      msg <- sprintf("%s. No matching base class.", error_base)
      stop(msg, call. = FALSE)
    }
    base_classes[[candidate]]
  } else if (is.character(x) && length(x) == 1) {
    if (x %in% names(base_classes)) {
      base_classes[[x]]
    } else if (x %in% names(base_unions)) {
      base_unions[[x]]
    } else {
      stop(sprintf("%s. No base classes are called '%s'", error_base, x), call. = FALSE)
    }
  } else {
    stop(sprintf("%s. Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base constructor function, not a %s.", error_base, obj_desc(x)), call. = FALSE)
  }
}

as_S4_class <- function(x, error_base) {
  # Silence R CMD check false postives
  distance <- subClass <- className <- package <- NULL

  # Convert generator function to class
  if (methods::is(x, "classGeneratorFunction")) {
    return(as_S4_class(methods::getClass(as.character(x@className)), error_base))
  }

  if (methods::is(x, "ClassUnionRepresentation")) {
    subclasses <- Filter(function(y) y@distance == 1, x@subclasses)
    subclasses <- lapply(subclasses, function(x) methods::getClass(x@subClass))
    do.call("new_union", subclasses)
  } else if (methods::extends(x, "oldClass")) {
    new_S3_class(as.character(x@className))
  } else if (methods::is(x, "classRepresentation")) {
    if (x@package == "methods" && x@className %in% names(base_classes)) {
      # Convert S4 representation of base types to R7 representation
      base_classes[[x@className]]
    } else if (x@package == "methods" && x@className == "NULL") {
      NULL
    } else {
      x
    }
  } else {
    stop(sprintf("%s. Unsupported S4 object: must be a class generator or a class definition, not a %s.", error_base, obj_desc(x)), call. = FALSE)
  }
}

is_S4_class <- function(x) inherits(x, "classRepresentation")

class_type <- function(x) {
  if (is.null(x)) {
    "NULL"
  } else if (is_base_class(x)) {
    "R7_base"
  } else if (is_class(x)) {
    "R7"
  } else if (is_union(x)) {
    "R7_union"
  } else if (is_S3_class(x)) {
    "R7_S3"
  } else if (is_S4_class(x)) {
    "S4"
  } else {
    stop("`x` is not standard R7 class", call. = FALSE)
  }
}

class_friendly <- function(x) {
  switch(class_type(x),
    NULL = "NULL",
    S4 = "an S4 class",
    R7 = "an R7 class",
    R7_base = "a base type",
    R7_union = "an R7 union",
    R7_S3 = "an S3 class",
  )
}

class_constructor <- function(.x, ...) {
  switch(class_type(.x),
    NULL = function() NULL,
    S4 = function(...) methods::new(.x, ...),
    R7 = .x,
    R7_base = .x$constructor,
    R7_union = class_constructor(.x$classes[[1]]),
    R7_S3 = .x$constructor,
  )
}
class_construct <- function(.x, ...) {
  class_constructor(.x)(...)
}

class_validate <- function(class, object) {
  validator <- switch(class_type(class),
    NULL = NULL,
    S4 = methods::validObject,
    R7 = class@validator,
    R7_base = class$validator,
    R7_union = NULL,
    R7_S3 = class$validator,
  )

  if (is.null(validator)) {
    NULL
  } else {
    validator(object)
  }
}

class_desc <- function(x) {
  switch(class_type(x),
    NULL = "<ANY>",
    S4 = paste0("S4<", x@className, ">"),
    R7 = paste0("<", x@name, ">"),
    R7_base = paste0("<", x$class, ">"),
    R7_union = oxford_or(unlist(lapply(x$classes, class_desc))),
    R7_S3 = paste0("S3<", paste0(x$class, collapse = "/"), ">"),
  )
}

# Vector of class names; used in method introspection
class_dispatch <- function(x) {
  switch(class_type(x),
    NULL = NULL,
    S4 = S4_strip_union(methods::extends(x)),
    R7 = c(x@name, class_dispatch(x@parent)),
    R7_base = c("R7_object", x$class),
    R7_S3 = c("R7_object", x$class),
    stop("Unsupported")
  )
}

# Class name when registering an R7 method
class_register <- function(x) {
  switch(class_type(x),
    NULL = "NULL",
    S4 = as.character(x@className),
    R7 = x@name,
    R7_base = x$class,
    R7_S3 = x$class[[1]],
    stop("Unsupported")
  )
}

# Used when printing method signature to generate executable code
class_deparse <- function(x) {
  switch(class_type(x),
    NULL = "",
    S4 = as.character(x@className),
    R7 = x@name,
    R7_base = encodeString(x$class, quote = '"'),
    R7_union = {
      classes <- vcapply(x$classes, class_deparse)
      paste0("new_union(", paste(classes, collapse = ", "), ")")
    },
    R7_S3 = paste0("new_S3_class(", deparse1(x$class), ")"),
  )
}

class_inherits <- function(x, what) {
  switch(class_type(what),
    NULL = TRUE,
    S4 = isS4(x) && methods::is(x, what),
    R7 = inherits(x, "R7_object") && inherits(x, what@name),
    R7_base = what$class %in% .class2(x),
    R7_union = any(vlapply(what$classes, class_inherits, x = x)),
    R7_S3 = !isS4(x) && is_prefix(what$class, class(x)),
  )
}

obj_type <- function(x) {
  if (is.null(x)) {
    "NULL"
  } else if (inherits(x, "R7_object")) {
    "R7"
  } else if (isS4(x)) {
    "S4"
  } else if (is.object(x)) {
    "S3"
  } else {
    "base"
  }
}
obj_desc <- function(x) {
  switch(obj_type(x),
   NULL = "NULL",
   base = paste0("<", typeof(x), ">"),
   S3 = paste0("S3<", paste(class(x), collapse = "/"), ">"),
   S4 = paste0("S4<", class(x), ">"),
   R7 = paste0("<", class(x)[[1]], ">")
  )
}
obj_dispatch <- function(x) {
  switch(obj_type(x),
    NULL = "NULL",
    base = .class2(x),
    S3 = class(x),
    S4 = S4_strip_union(methods::is(x)),
    R7 = class(x) # = class_dispatch(object_class(x))
  )
}

# R7 handles unions at method registration time, where as S4 handles them at
# dispatch time.
S4_strip_union <- function(class_names) {
  classes <- lapply(class_names, methods::getClass)
  is_union <- vlapply(classes, methods::is, "ClassUnionRepresentation")

  setdiff(class_names[!is_union], "vector")
}

# helpers -----------------------------------------------------------------

# Suppress @className false positive
globalVariables("className")
