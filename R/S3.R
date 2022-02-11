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
  } else {
    constructor <- function() {
      stop(sprintf("S3 class <%s> doesn't have a constructor", class[[1]]), call. = FALSE)
    }
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
