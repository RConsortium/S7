#' @export
prop <- function(obj, name) {
  attr(obj, name, exact = TRUE)
}

#' @export
`prop<-` <- function(obj, name, value) {
  attr(obj, name) <- value
  validate(obj)

  invisible(obj)
}

#' @export
`@` <- function(obj, name) {
  if (!inherits(obj, "object")) {
    return(base::`@`(obj, name))
  }

  nme <- as.character(substitute(name))
  prop(obj, nme)
}

#' @export
`@<-.object` <- function(obj, name, value) {
  if (!inherits(obj, "object")) {
    return(base::`@<-`(obj, name))
  }

  nme <- as.character(substitute(name))
  prop(obj, nme) <- value

  invisible(obj)
}
