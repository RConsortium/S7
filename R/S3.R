#' Declare an S3 class
#'
#' The S3 class system is informal so lacks a formal specification of
#' inheritance and expected attributes. These are needed for to use S3
#' objects with R7, so `s3_class()` gives you a way to formally specify
#' the properties of the S3 class.
#'
#' # Simple uses
#' Three ways of using S3 with R7 one require you to specify the S3 class
#' vector:
#'
#' * Registering a S3 method for an R7 generic.
#' * Restricting an R7 property to an S3 class.
#' * Using an an S3 class in R7 union.
#'
#' This is typically straightforward to determine and supply:
#'
#' ```R
#' method(my_generic, s3_class("factor")) <- function(x) "A factor"
#' new_class("my_class", properties = list(types = s3_class("factor")))
#' new_union("character", s3_class("factor"))
#' ```
#'
#' # Extending an S3 class
#'
#' Creating an R7 class that extends an S3 class requires more work. You'll
#' also need to provide a constructor for the S3 class that follows R7
#' conventions. This means the first argument should be `.data`, and it
#' should be followed by one argument for each attribute used by the class.
#'
#' This is often challenging because S3 classes typically quite heavily wrapped
#' for user convenience. For example, the factor class is an integer vector
#' with a character vector of `levels`, but there's no base R function that
#' takes an integer vector of values and character vector of levels and creates
#' a factor object.
#'
#' You may optionally want to also provide a `validator` function which will
#' ensure the [validate()] confirms the validity of any R7 classes that build
#' on this class. Unlike an R7 validator, you are responsible for validating
#' the types of the attributes.
#'
#' @export
#' @param class Character vector of S3 classes
#' @param constructor An optional constructor that can be used to create
#'   objects of the specified class. This is only needed if you wish to
#'   have an R7 class inherit from an S3 class. It must be specified in the
#'   same way as a R7 constructor: the first argument should be `.data`
#'   (the base type whose attributes will be modified).
#' @param validator An optional validator that can be used to check that
#'   the S3 object is correct.
s3_class <- function(class, constructor = NULL, validator = NULL) {
  if (!is.character(class)) {
    stop("`class` must be a character vector", call. = FALSE)
  }
  if (!is.null(constructor)) {
    check_constructor(constructor)
  } else {
    constructor <- function() {
      stop(sprintf("S3 class <%s> doesn't have a constructor", class[[1]]), call. = FALSE)
    }
  }

  structure(
    list(
      class = class,
      constructor = constructor,
      validator = validator
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

# -------------------------------------------------------------------------
# Define a few base examples

s3_factor <- s3_class("factor",
  function(.data, levels) {
    structure(.data, levels = levels, class = "factor")
  },
  function(object) {
    c(
      if (typeof(object) != "integer")
        "Underlying data must be an <integer>",
      if (!is.character(attr(object, "levels")))
        "attr(, 'levels') must be a character vector"
    )
  }
)

s3_Date <- s3_class("Date",
  function(.data) {
    .Date(.data)
  },
  function(object) {
    if (!is.numeric(object)) {
      "Underlying data must be numeric"
    }
  }
)

s3_POSIXct <- s3_class(
  c("POSIXct", "POSIXt"),
  function(.data, tz = "") {
    .POSIXct(.data, tz = tz)
  },
  function(object) {
    c(
      if (!is.numeric(object))
        "Underlying data must be numeric",
      if (!is.character(tz) || length(tz) != 1)
        "attr(, 'tz') must be a single string"
    )
  }
)

s3_data.frame <- s3_class("data.frame",
  function(.data, row.names = NULL) {
    if (is.null(row.names)) {
      list2DF(.data)
    } else {
      out <- list2DF(.data, length(row.names))
      attr(out, "row.names") <- row.names
      out
    }
  },
  function(object) {
    c(
      if (!is.list(.data))
        "Underlying data must be a <list>",
      if (!is.character(row.names) || !is.integer(row.names) || !is.null(row.names))
        "attr(, 'rownames') must be a character vector, integer vector, or NULL"
    )
  }
)
