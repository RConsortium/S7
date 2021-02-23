#' Extract or replace a property
#'
#' @param obj An object from a R7 class
#' @param name The name of the parameter as a character. No partial matching is done.
#' @param value A replacement value for the parameter. The object is
#'   automatically checked for validity after the replacement is done.
#' @export
property <- function(obj, name) {
  val <- property_safe(obj, name)
  if (is.null(val)) {
    class <- object_class(obj)
    stop(sprintf("`%s` objects do not have a `%s` property", class@name, name), call. = FALSE)
  }
  val
}

property_safe <- function(obj, name, otherwise = NULL) {
  if (identical(name, ".data")) {
    # Remove properties, return the rest
    props <- properties(obj)
    for (name in names(props)) {
      attr(obj, name) <- NULL
    }
    class(obj) <- setdiff(class_names(property_safe(object_class(obj), "parent")), "r7_object")
    object_class(obj) <- NULL
    return(obj)
  }
  val <- attr(obj, name, exact = TRUE)
  if (is.null(val)) {
    return(otherwise)
  }
  val
}

properties <- function(object) {
  obj_class <- object_class(object)
  prop <- list()
  while(!is.null(obj_class)) {
    prop <- c(obj_class@properties, prop)
    obj_class <- property_safe(obj_class, "parent")
  }
  prop
}

#' @rdname property
#' @export
`property<-` <- function(obj, name, value) {
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

#' @rdname property
#' @usage obj@name
#' @export
`@` <- function(obj, name) {
  if (!inherits(obj, "r7_object")) {
    name <- substitute(name)
    return(do.call(base::`@`,list(obj, name)))
  }

  nme <- as.character(substitute(name))
  property(obj, nme)
}

#' @rawNamespace S3method("@<-",r7_object)
`@<-.r7_object` <- function(obj, name, value) {
  if (!inherits(obj, "r7_object")) {
    return(base::`@<-`(obj, name))
  }

  nme <- as.character(substitute(name))
  property(obj, nme) <- value

  invisible(obj)
}
