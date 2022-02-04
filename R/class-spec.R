#' Standard class specifications
#'
#' This is used as the interface between R7 and R's other OO systems, allowing
#' you to use R7 classes and methods with base types, informal S3 classes, and
#' formal S4 classes.
#'
#' @param x A class specification.
#'    * An R7 class object or class union.
#'    * An S3 class object, created by `s3_class()`.
#'    * An S4 class object.
#'    * A base type specified either with its constructor (`logical`, `integer`,
#'      `double` etc) or its name (`"logical"`, `"integer"`, "`double`" etc).
#' @param arg Argument name used when generating errors.
#' @export
#' @return A standardised class: either `NULL`, an R7 class, an R7 union,
#'   as [s3_class], or a S4 class.
as_class <- function(x, arg = deparse(substitute(x))) {
  error_base <- sprintf("Can't convert `%s` to a valid class", arg)

  if (is.null(x)) {
    x
  } else if (is_class(x)) {
    x
  } else if (is_union(x)) {
    x
  } else if (is_s3_class(x)) {
    x
  } else if (isS4(x) && methods::is(x, "classGeneratorFunction")) {
    methods::getClass(as.character(x@className))
  } else if (isS4(x) && methods::is(x, "classRepresentation")) {
    x
  } else if (is.function(x)) {
    candidate <- Filter(function(y) identical(x, y), base_constructors)
    if (length(candidate) == 0) {
      stop(sprintf("%s. Could not find base class corresponding to supplied constructor function", error_base), call. = FALSE)
    }
    base_classes[[names(candidate)[[1]]]]
  } else if (is.character(x) && length(x) == 1) {
    if (x %in% names(base_classes)) {
      base_classes[[x]]
    } else {
      stop(sprintf("%s. No base classes are called '%s'", error_base, x), call. = FALSE)
    }
  } else {
    stop(sprintf("%s. Class specification must be an R7 class object, the result of `s3_class()`, an S4 class object, or a base constructor function, not a %s.", error_base, obj_desc(x)), call. = FALSE)
  }
}



class_type <- function(x) {
  if (is_class(x)) {
    if (utils::hasName(base_classes, x@name)) {
      "r7_base"
    } else {
      "r7"
    }
  } else if (is_union(x)) {
    "r7_union"
  } else if (is.null(x)) {
    "NULL"
  } else if (is_s3_class(x)) {
    "s3"
  } else if (isS4(x)) {
    "s4"
  } else {
    stop("`x` is not standard R7 class", call. = FALSE)
  }
}

class_desc <- function(x) {
  switch(class_type(x),
    NULL = "<ANY>",
    s3 = fmt_classes(x[[1]]),
    s4 = fmt_classes(x@className),
    r7 = fmt_classes(x@name),
    r7_base = fmt_classes(x@name),
    r7_union = paste(unlist(lapply(x@classes, class_desc)), collapse = " u "),
  )
}

# Used when printing method signature to generate executable code
class_deparse <- function(x) {
  switch(class_type(x),
    NULL = "",
    s3 = paste0("s3_class(", paste(encodeString(x, quote = '"'), collapse = ", "), ")"),
    s4 = as.character(x@className),
    r7 = x@name,
    r7_base = encodeString(x@name, quote = '"'),
    r7_union = {
      classes <- vcapply(x@classes, class_deparse)
      paste0("new_union(", paste(classes, collapse = ", "), ")")
    }
  )
}

class_inherits <- function(x, what) {
  switch(class_type(what),
    NULL = TRUE,
    s3 = !isS4(x) && inherits(x, what[[1]]),
    s4 = isS4(x) && methods::is(x, what),
    r7 = inherits(x, "R7_object") && inherits(x, what@name),
    r7_base = what@name %in% .class2(x),
    r7_union = any(vlapply(what@classes, class_inherits, x = x))
  )
}

obj_type <- function(x) {
  if (is.null(x)) {
    "NULL"
  } else if (inherits(x, "R7_object")) {
    "r7"
  } else if (isS4(x)) {
    "s4"
  } else if (is.object(x)) {
    "s3"
  } else {
    "base"
  }
}
obj_desc <- function(x) {
  switch(obj_type(x),
   NULL = "NULL",
   base = fmt_classes(typeof(x)),
   s3 = fmt_classes(class(x)[[1]]),
   s4 = fmt_classes(class(x)),
   r7 = fmt_classes(object_class(x)@name)
  )
}

#' Declare an S3 class vector
#'
#' The S3 class system is informal so doesn't have a way to formally register
#' a class. This helper allows you to use S3 classes within R7.
#'
#' @export
#' @param class Character vector of S3 classes
s3_class <- function(class) {
  if (!is.character(class)) {
    stop("`class` must be a character vector", call. = FALSE)
  }
  structure(class, class = "r7_s3_class")
}

is_s3_class <- function(x) {
  inherits(x, "r7_s3_class")
}

# helpers -----------------------------------------------------------------

fmt_classes <- function(classes, collapse = ", ") {
  paste0("<", classes, ">", collapse = collapse)
}

# Suppress @className false positive
globalVariables("className")
