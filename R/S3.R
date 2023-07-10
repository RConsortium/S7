#' Declare an S3 class
#'
#' To use an S3 class with S7, you must explicitly declare it using
#' `new_S3_class()` because S3 lacks a formal class definition.
#'
#' # Method dispatch, properties, and unions
#' There are three ways of using S3 with S7 that only require the S3 class
#' vector:
#'
#' * Registering a S3 method for an S7 generic.
#' * Restricting an S7 property to an S3 class.
#' * Using an S3 class in an S7 union.
#'
#' This is easy, and you can usually include the `new_S3_class()`
#' call inline:
#'
#' ```R
#' method(my_generic, new_S3_class("factor")) <- function(x) "A factor"
#' new_class("my_class", properties = list(types = new_S3_class("factor")))
#' new_union("character", new_S3_class("factor"))
#' ```
#'
#' # Extending an S3 class
#'
#' Creating an S7 class that extends an S3 class requires more work. You'll
#' also need to provide a constructor for the S3 class that follows S7
#' conventions. This means the first argument to the constructor should be
#' `.data`, and it should be followed by one argument for each attribute used
#' by the class.
#'
#' This can be awkward because base S3 classes are usually heavily wrapped for user
#' convenience and no low level constructor is available. For example, the
#' factor class is an integer vector with a character vector of `levels`, but
#' there's no base R function that takes an integer vector of values and
#' character vector of levels, verifies that they are consistent, then
#' creates a factor object.
#'
#' You may optionally want to also provide a `validator` function which will
#' ensure that [validate()] confirms the validity of any S7 classes that build
#' on this class. Unlike an S7 validator, you are responsible for validating
#' the types of the attributes.
#'
#' The following code shows how you might wrap the base Date class.
#' A Date is a numeric vector with class `Date` that can be constructed with
#' `.Date()`.
#'
#' ```R
#' S3_Date <- new_S3_class("Date",
#'   function(.data = integer()) {
#'     .Date(.data)
#'   },
#'   function(self) {
#'     if (!is.numeric(self)) {
#'       "Underlying data must be numeric"
#'     }
#'   }
#' )
#' ```
#'
#' @export
#' @param class S3 class vector (i.e. what `class()` returns). For method
#'   registration, you can abbreviate this to a single string, the S3 class
#'   name.
#' @param constructor An optional constructor that can be used to create
#'   objects of the specified class. This is only needed if you wish to
#'   have an S7 class inherit from an S3 class. It must be specified in the
#'   same way as a S7 constructor: the first argument should be `.data`
#'   (the base type whose attributes will be modified).
#'
#'   All arguments to the constructor should have default values so that
#'   when the constructor is called with no arguments, it returns returns
#'   an "empty", but valid, object.
#' @param validator An optional validator used by [validate()] to check that
#'   the S7 object adheres to the constraints of the S3 class.
#'
#'   A validator is a single argument function that takes the object to
#'   validate and returns `NULL` if the object is valid. If the object is
#'   invalid, it returns a character vector of problems.
#' @returns An S7 definition of an S3 class, i.e. a list with class
#'   `S7_S3_class`.
#' @examples
#' # No checking, just used for dispatch
#' Date <- new_S3_class("Date")
#'
#' my_generic <- new_generic("my_generic", "x")
#' method(my_generic, Date) <- function(x) "This is a date"
#'
#' my_generic(Sys.Date())
new_S3_class <- function(class, constructor = NULL, validator = NULL) {
  if (!is.character(class)) {
    stop("`class` must be a character vector", call. = FALSE)
  }
  if (!is.null(constructor)) {
    check_S3_constructor(constructor)
  } else {
    constructor <- function(.data) {
      stop(sprintf("S3 class <%s> doesn't have a constructor", class[[1]]), call. = FALSE)
    }
  }

  out <- list(
    class = class,
    constructor = constructor,
    validator = validator
  )
  class(out) <- "S7_S3_class"
  out
}

#' @export
print.S7_S3_class <- function(x, ...) {
  cat("<S7_S3_class>: ", class_desc(x), "\n", sep = "")
  invisible(x)
}

#' @export
str.S7_S3_class <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object, ..., nest.lev = nest.lev)
}

check_S3_constructor <- function(constructor) {
  arg_names <- names(formals(constructor))
  if (arg_names[[1]] != ".data") {
    stop("First argument to `constructor` must be .data", call. = FALSE)
  }
  if ("..." %in% arg_names) {
    stop("`constructor` can not use `...`", call. = FALSE)
  }
}

is_S3_class <- function(x) {
  inherits(x, "S7_S3_class")
}

# -------------------------------------------------------------------------
# Pull out validation functions so hit by code coverage

validate_factor <- function(self) {
  c(
    if (typeof(self) != "integer")
      "Underlying data must be an <integer>",
    if (!is.character(attr(self, "levels")))
      "attr(, 'levels') must be a <character>"
  )
}

validate_date <- function(self) {
  if (!is.numeric(self)) {
    "Underlying data must be numeric"
  }
}

validate_POSIXct <- function(self) {
  if (!is.numeric(self)) {
    return("Underlying data must be numeric")
  }

  tz <- attr(self, "tz")
  if (!is.character(tz) || length(tz) != 1) {
    return("attr(, 'tz') must be a single string")
  }
}

validate_data.frame <- function(self) {
  if (!is.list(self)) {
    return("Underlying data must be a <list>")
  }

  if (length(self) >= 1) {
    # Avoid materialising compact row names
    ns <- unique(c(lengths(self), .row_names_info(self, 2L)))
    if (length(ns) > 1) {
      return("All columns and row names must have the same length")
    }

    if (is.null(names(self))) {
      return("Underlying data must be named")
    }
  }
}

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 3
class_factor <- new_S3_class("factor",
  constructor = function(.data = integer(), levels = character()) {
    structure(.data, levels = levels, class = "factor")
  },
  validator = validate_factor
)

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 3
class_Date <- new_S3_class("Date",
  constructor = function(.data = double()) {
    .Date(.data)
  },
  validator = validate_date
)

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 3
class_POSIXct <- new_S3_class("POSIXct",
  constructor = function(.data = double(), tz = "") {
    .POSIXct(.data, tz = tz)
  },
  validator = validate_POSIXct
)

#' @export
#' @rdname base_classes
#' @format NULL
#' @order 3
class_data.frame <- new_S3_class("data.frame",
  constructor = function(.data = list(), row.names = NULL) {
    if (is.null(row.names)) {
      list2DF(.data)
    } else {
      out <- list2DF(.data, length(row.names))
      attr(out, "row.names") <- row.names
      out
    }
  },
  validator = validate_data.frame
)
