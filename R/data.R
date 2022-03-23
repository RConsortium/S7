#' Get/set underlying "base" data
#'
#' When an R7 class inherits from an existing base type, it can be useful
#' to work with the underlying object, i.e. the R7 object stripped of class
#' and all properties.
#'
#' @inheritParams prop
#' @param value Object used to replace the underlying data.
#' @export
#' @examples
#' text <- new_class("text", parent = class_character)
#' y <- text(c(foo = "bar"))
#' y
#' R7_data(y)
#'
#' R7_data(y) <- c("a", "b")
#' y
R7_data <- function(object) {
  check_R7_inherits(object)

  zap_attr(object, c(prop_names(object), "class", "R7_class"))
}

#' @export
#' @rdname R7_data
`R7_data<-` <- function(object, check = TRUE, value) {
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
