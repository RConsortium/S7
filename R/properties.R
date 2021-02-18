#' Extract or replace a property
#'
#' @param obj An object from a R7 class
#' @param name The name of the parameter as a character. No partial matching is done.
#' @param value A replacement value for the parameter. The object is
#'   automatically checked for validity after the replacement is done.
#' @export
prop <- function(obj, name) {
  if (identical(name, ".data")) {
    obj2 <- obj
    attributes(obj2) <- NULL
    return(obj2)
  }
  attr(obj, name, exact = TRUE)
}

#' @rdname prop
#' @export
`prop<-` <- function(obj, name, value) {
  if (name == ".data") {
    attrs <- attributes(obj)
    obj <- value
    attributes(obj) <- attrs
  } else {
    attr(obj, name) <- value
  }

  validate(obj)

  invisible(obj)
}

#' @rdname prop
#' @usage obj@name
#' @export
`@` <- function(obj, name) {
  if (!inherits(obj, "r7_object")) {
    return(base::`@`(obj, name))
  }

  nme <- as.character(substitute(name))
  prop(obj, nme)
}

#' @rawNamespace S3method("@<-",r7_object)
`@<-.r7_object` <- function(obj, name, value) {
  if (!inherits(obj, "r7_object")) {
    return(base::`@<-`(obj, name))
  }

  nme <- as.character(substitute(name))
  prop(obj, nme) <- value

  invisible(obj)
}
