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
#'   * A "special", either [missing_class] or [any_class].
#' @param arg Argument name used when generating errors.
#' @export
#' @return A standardised class: either `NULL`, an R7 class, an R7 union,
#'   as [new_S3_class], or a S4 class.
as_class <- function(x, arg = deparse(substitute(x))) {
  error_base <- sprintf("Can't convert `%s` to a valid class. ", arg)

  if (is.null(x)) {
    # NULL is handled specially because you can't assign a class to it,
    # so it can't be wrapped in new_base_class
    x
  } else if (is_foundation_class(x)) {
    x
  } else if (isS4(x)) {
    S4_to_R7_class(x, error_base)
  } else if (is.function(x)) {
    candidate <- find_base_name(x, names(base_classes))
    if (is.na(candidate)) {
      stop(paste0(error_base, "No matching base class."), call. = FALSE)
    }
    base_classes[[candidate]]
  } else if (is.character(x) && length(x) == 1) {
    if (x %in% names(base_classes)) {
      base_classes[[x]]
    } else if (x %in% names(base_unions)) {
      base_unions[[x]]
    } else {
      msg <- sprintf("No base classes are called '%s'", x)
      stop(paste0(error_base, msg), call. = FALSE)
    }
  } else {
    msg <- sprintf("Class specification must be an R7 class object, the result of `new_S3_class()`, an S4 class object, or a base constructor function, not a %s.", obj_desc(x))
    stop(paste0(error_base, msg), call. = FALSE)
  }
}

is_foundation_class <- function(x) {
  is_class(x) ||
    is_union(x) ||
    is_base_class(x) ||
    is_S3_class(x) ||
    is_missing_class(x) ||
    is_any_class(x)
}

class_type <- function(x) {
  if (is.null(x)) {
    "NULL"
  } else if (is_missing_class(x)) {
    "missing"
  } else if (is_any_class(x)) {
    "any"
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
    missing = "a missing argument",
    any = "any type",
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
    any = function() NULL,
    S4 = function(...) methods::new(.x, ...),
    R7 = .x,
    R7_base = .x$constructor,
    R7_union = class_constructor(.x$classes[[1]]),
    R7_S3 = .x$constructor,
    stop(sprintf("Can't construct %s", class_friendly(.x)), call. = FALSE)
  )
}
class_construct <- function(.x, ...) {
  class_constructor(.x)(...)
}

class_validate <- function(class, object) {
  validator <- switch(class_type(class),
    S4 = methods::validObject,
    R7 = class@validator,
    R7_base = class$validator,
    R7_S3 = class$validator,
    NULL
  )

  if (is.null(validator)) {
    NULL
  } else {
    validator(object)
  }
}

class_desc <- function(x) {
  switch(class_type(x),
    NULL = "<NULL>",
    missing = "<MISSING>",
    any = "<ANY>",
    S4 = paste0("S4<", x@className, ">"),
    R7 = paste0("<", R7_class_name(x), ">"),
    R7_base = paste0("<", x$class, ">"),
    R7_union = oxford_or(unlist(lapply(x$classes, class_desc))),
    R7_S3 = paste0("S3<", paste0(x$class, collapse = "/"), ">"),
  )
}

# Vector of class names; used in method introspection
class_dispatch <- function(x) {
  if (is_class(x) && x@name == "R7_object") {
    return(c("R7_object", "ANY"))
  }

  switch(class_type(x),
    NULL = c("NULL", "ANY"),
    missing = "MISSING",
    any = "ANY",
    S4 = c(S4_class_dispatch(methods::extends(x)), "ANY"),
    R7 = c(R7_class_name(x), class_dispatch(x@parent)),
    R7_base = c(x$class, "R7_object", "ANY"),
    R7_S3 = c(x$class, "R7_object", "ANY"),
    stop("Unsupported")
  )
}

# Class name when registering an R7 method
class_register <- function(x) {
  switch(class_type(x),
    NULL = "NULL",
    missing = "MISSING",
    any = "ANY",
    S4 = S4_class_name(x),
    R7 = R7_class_name(x),
    R7_base = x$class,
    R7_S3 = x$class[[1]],
    stop("Unsupported")
  )
}

# Used when printing method signature to generate executable code
class_deparse <- function(x) {
  switch(class_type(x),
    "NULL" = "NULL",
    missing = "missing_class",
    any = "any_class",
    S4 = as.character(x@className),
    R7 = R7_class_name(x),
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
    "NULL" = is.null(x),
    missing = FALSE,
    any = TRUE,
    S4 = isS4(x) && methods::is(x, what),
    R7 = inherits(x, "R7_object") && inherits(x, R7_class_name(what)),
    R7_base = what$class == base_class(x),
    R7_union = any(vlapply(what$classes, class_inherits, x = x)),
    R7_S3 = !isS4(x) && is_prefix(what$class, class(x)),
  )
}

obj_type <- function(x) {
  if (inherits(x, "R7_object")) {
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
   base = paste0("<", typeof(x), ">"),
   S3 = paste0("S3<", paste(class(x), collapse = "/"), ">"),
   S4 = paste0("S4<", class(x), ">"),
   R7 = paste0("<", class(x)[[1]], ">")
  )
}
obj_dispatch <- function(x) {
  switch(obj_type(x),
    base = c(base_class(x), "ANY"),
    S3 = c(class(x), "ANY"),
    S4 = c(S4_class_dispatch(methods::getClass(class(x))), "ANY"),
    R7 = c(class(x), "ANY") # = class_dispatch(R7_class(x))
  )
}

base_class <- function(x) {
  switch(typeof(x),
    closure = "function",
    special = "function",
    builtin = "function",
    language = "call",
    typeof(x)
  )
}

# helpers -----------------------------------------------------------------

# Suppress @className false positive
globalVariables("className")
