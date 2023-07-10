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
#' text <- new_class("text", parent = class_character)
#' y <- text(c(foo = "bar"))
#' y
#' S7_data(y)
#'
#' S7_data(y) <- c("a", "b")
#' y
S7_data <- function(object) {
  check_is_S7(object)

  zap_attr(object, c(prop_names(object), "class", "S7_class"))
}

#' @export
#' @rdname S7_data
`S7_data<-` <- function(object, check = TRUE, value) {
  attrs <- attributes(object)
  object <- value
  attributes(object) <- attrs
  if (isTRUE(check)) {
    validate(object)
  }
  return(invisible(object))
}


zap_attr <- function(x, names) {
  for (name in names) {
    attr(x, name) <- NULL
  }
  x
}
