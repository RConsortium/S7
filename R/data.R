#' Get/set underlying "base" data
#'
#' When an S7 class inherits from an existing base type, it can be useful
#' to work with the underlying object, i.e. the S7 object stripped of class
#' and properties.
#'
#' @inheritParams prop
#' @param value Object used to replace the underlying data.
#' @return `S7_data()` returns the data stored in the base object;
#'   `S7_data<-()` is called for its side-effects and returns `object`
#'   invisibly.
#' @export
#' @examples
#' Text <- new_class("Text", parent = class_character)
#' y <- Text(c(foo = "bar"))
#' y
#' S7_data(y)
#'
#' S7_data(y) <- c("a", "b")
#' y
S7_data <- function(object) {
  check_is_S7(object)
  check_not_environment(object, "S7_data()")

  zap_attr(object, c(prop_names(object), "class", "S7_class"))
}

#' @export
#' @rdname S7_data
`S7_data<-` <- function(object, check = TRUE, value) {
  check_not_environment(object, "S7_data<-")
  attrs <- attributes(object)
  object <- value
  attributes(object) <- attrs
  if (isTRUE(check)) {
    validate(object)
  }
  return(invisible(object))
}

check_not_environment <- function(object, fn) {
  if (!is.environment(object)) {
    return(invisible())
  }
  stop(
    sprintf(
      "Can't call `%s` on an environment because attribute changes are made in place.",
      fn
    ),
    call. = FALSE
  )
}


zap_attr <- function(x, names) {
  for (name in names) {
    attr(x, name) <- NULL
  }
  x
}
