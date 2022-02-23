#' Get/set underlying "base" data
#'
#' When an R7 class inherits from an existing base type, it can be useful
#' to work with the underlying object, i.e. the R7 object stripped of class
#' and all properties.
#'
#' @inheritParams prop
#' @param value Object used to replace the underlying data.
#' @export
#' @keywords internal
#' @examples
#' text <- new_class("text", parent = "character")
#' y <- text(c(foo = "bar"))
#' str(R7_data(y))
R7_data <- function(object) {
  check_R7(object)

  # Remove properties, return the rest
  for (name in prop_names(object)) {
    attr(object, name) <- NULL
  }
  attr(object, "R7_class") <- NULL
  class(object) <- NULL

  object
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
