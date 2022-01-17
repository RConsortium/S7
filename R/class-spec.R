#' Standard class specifications
#'
#' This is used as the interface between R7 and R's other OO systems, allowing
#' you to build R7 classes and methods on top of base types, informal S3
#' classes, and formal S4 classes.
#'
#' @param x A class specification.
#'    * An R7 class object or class union.
#'    * An S3 class object, created by `s3_class()`.
#'    * An S4 class object.
#'    * A base type specified either with its constructor (`logical`, `integer`,
#'      `double` etc) or its name (`"logical"`, `"integer"`, "`double`" etc).
#' @param arg_name Argument name used when generating errors.
#' @export
as_class <- function(x, arg_name = "as_class()") {
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
    if (length(candidate)  == 0) {
      stop(sprintf("%s: could not find base class corresponding to supplied constructor function", arg_name), call. = FALSE)
    }
    base_classes[[names(candidate)[[1]]]]
  } else if (is.character(x) && length(x) == 1) {
    if (x %in% names(base_classes)) {
      base_classes[[x]]
    } else {
      stop(sprintf("%s: Can't find base class called '%s'", arg_name, x), call. = FALSE)
    }
  } else {
    stop(sprintf("%s: class specification must by a R7 class object, result of s3_class(), a S4 class object, or a base constructor function, not a %s.", arg_name, obj_desc(x)), call. = FALSE)
  }
}

class_type <- function(x) {
  if (is_class(x)) {
    if (hasName(base_classes, x@name)) {
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
    stop("`x` is not standard R7 class")
  }
}

class_desc <- function(x) {
  switch(class_type(x),
    NULL = "",
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
    r7_base = inherits(x, what@name),
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
    stop("`class` must be a character vector")
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
