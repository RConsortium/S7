#' Extract or replace a property
#'
#' @param obj An object from a R7 class
#' @param name The name of the parameter as a character. No partial matching is done.
#' @param value A replacement value for the parameter. The object is
#'   automatically checked for validity after the replacement is done.
#' @export
prop <- function(obj, name) {
  attr(obj, name, exact = TRUE)
}

#' @rdname prop
#' @export
`prop<-` <- function(obj, name, value) {
  attr(obj, name) <- value
  validate(obj)

  invisible(obj)
}

#' @rdname prop
#' @export
`@` <- function(obj, name) {
  if (!inherits(obj, "object")) {
    return(base::`@`(obj, name))
  }

  nme <- as.character(substitute(name))
  prop(obj, nme)
}

#' @rdname prop
#' @export
`@<-.object` <- function(obj, name, value) {
  if (!inherits(obj, "object")) {
    return(base::`@<-`(obj, name))
  }

  nme <- as.character(substitute(name))
  prop(obj, nme) <- value

  invisible(obj)
}
