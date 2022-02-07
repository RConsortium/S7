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

class_constructor <- function(.x, ...) {
  switch(class_type(.x),
    NULL = function() NULL,
    s3 = {
      if (is.null(.x$constructor)) {
        stop(sprintf("S3 class <%s> doesn't have a constructor", .x$class[[1]]), call. = FALSE)
      }
      .x$constructor
    },
    s4 = function(...) methods::new(.x, ...),
    r7 = .x,
    r7_base = .x,
    r7_union = .x@classes[[1]],
  )
}
class_construct <- function(.x, ...) {
  class_constructor(.x)(...)
}

class_desc <- function(x) {
  switch(class_type(x),
    NULL = "<ANY>",
    s3 = fmt_classes(x$class[[1]]),
    s4 = fmt_classes(x@className),
    r7 = fmt_classes(x@name),
    r7_base = fmt_classes(x@name),
    r7_union = oxford_or(unlist(lapply(x@classes, class_desc))),
  )
}

# Used when printing method signature to generate executable code
class_deparse <- function(x) {
  switch(class_type(x),
    NULL = "",
    s3 = paste0("s3_class(", paste(encodeString(x$class, quote = '"'), collapse = ", "), ")"),
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
    s3 = !isS4(x) && is_prefix(what$class, class(x)),
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

#' Declare an S3 class
#'
#' @description
#' The S3 class system is informal so lacks a formal specification of
#' inheritance and expected attributes. These are needed for to use S3
#' objects with R7, so `s3_class()` gives you a way to formally specify
#' the properties of the S3 class.
#'
#' Validation is not currently supported.
#'
#' @details
#' There are four ways that you can use S3 with R7:
#'
#' * To provide an S3 method for an R7 generic.
#' * To restrict an R7 property to an S3 class.
#' * To use an S3 class in R7 union.
#' * To extend an S3 class with an R7 class.
#'
#' The first three uses only need the S3 class vector, which is straightforward
#' to determine and supply:
#'
#' ```R
#' method(my_generic, s3_class("factor")) <- function(x) "A factor"
#' new_class("my_class", properties = list(types = s3_class("factor")))
#' new_union("character", s3_class("factor"))
#' ```
#'
#' Creating an R7 class that inherits behaviour from an S3 method requires
#' more work. You'll need to provide a constructor for the S3 class that
#' follows R7 conventions. This means:
#'
#' * The first argument should always be `.data`.
#' * There should be one additional argument for each attribute used by the
#'   class.
#' * The constructor should check that all arguments have the correct types.
#'
#' This is often challenging because S3 classes typically quite heavily wrapped
#' for user convenience. For example, the factor class is an integer vector
#' with a character vector of `levels`, but there's no base R function that
#' takes an integer vector of values and character vector of levels and creates
#' a factor object.
#'
#' @export
#' @param class Character vector of S3 classes
#' @param constructor An optional constructor that can be used to create
#'   objects of the specified class. This is only needed if you wish to
#'   have an R7 class inherit from an S3 class. It must be specified in the
#'   same way as a R7 constructor: the first argument should be `.data`
#'   (the base type whose attributes will be modified).
s3_class <- function(class, constructor = NULL) {
  if (!is.character(class)) {
    stop("`class` must be a character vector", call. = FALSE)
  }
  if (!is.null(constructor)) {
    check_constructor(constructor)
  }

  structure(
    list(
      class = class,
      constructor = constructor
    ),
    class = "r7_s3_class"
  )
}

check_constructor <- function(constructor) {
  arg_names <- names(formals(constructor))
  if (arg_names[[1]] != ".data") {
    stop("First argument to `constructor` must be .data", call. = FALSE)
  }
  if ("..." %in% arg_names) {
    stop("`constructor` can not use ...", call. = FALSE)
  }
}

is_s3_class <- function(x) {
  inherits(x, "r7_s3_class")
}

s3_factor <- s3_class("factor", function(.data, levels) {
  stopifnot(is.integer(.data))
  stopifnot(is.character(levels))
  factor(.data, levels = levels)
})
s3_Date <- s3_class("Date", function(.data) {
  stopifnot(is.numeric(.data))
  .Date(.data)
})
s3_POSIXct <- s3_class(c("POSIXct", "POSIXt"), function(.data, tz = "") {
  stopifnot(is.numeric(.data))
  stopifnot(is.character(tz) && length(tz) == 1)
  .POSIXct(.data, tz = tz)
})
s3_data.frame <- s3_class("data.frame", function(.data, row.names = NULL) {
  stopifnot(is.list(.data))
  stopifnot(is.character(row.names) || is.integer(row.names) || is.null(row.names))

  if (is.null(row.names)) {
    list2DF(.data)
  } else {
    out <- list2DF(.data, length(row.names))
    attr(out, "row.names") <- row.names
    out
  }
})

# helpers -----------------------------------------------------------------

fmt_classes <- function(classes, collapse = ", ") {
  paste0("<", classes, ">", collapse = collapse)
}

# Suppress @className false positive
globalVariables("className")
