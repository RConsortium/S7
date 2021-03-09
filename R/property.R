#' Define a new property
#'
#' @param name The name of the property
#' @param class The class of the property
#' @param getter An optional function used to get the value. The function
#'   should take the object as its sole argument and return the value. If the
#'   property has a `class` the class of the value is validated.
#' @param setter An optional function used to set the value. The function
#'   should take the object and new value as its two parameters and return the
#'   modified object. The value is _not_ automatically checked.
#' @export
property_new <- function(name, class = NULL, getter = NULL, setter = NULL) {
  out <- list(name = name, class = class, getter = getter, setter = setter)
  class(out) <- "R7_property"

  out
}

#' Extract or replace a property
#'
#' - [property] or the shorthand `@` extracts a given property, throwing an error if the property doesn't exist for that object.
#' - [property_safely] returns `NULL` if a property doesn't exist, rather than throwing an error.
#' - [property<-] assigns a new value for a given property.
#' @param object An object from a R7 class
#' @param name The name of the parameter as a character. No partial matching is done.
#' @param value A replacement value for the parameter. The object is
#'   automatically checked for validity after the replacement is done.
#' @export
property <- function(object, name) {
  val <- property_safely(object, name)
  if (is.null(val)) {
    class <- object_class(object)
    stop(sprintf("Can't find property '%s' in <%s>", name, class@name), call. = FALSE)
  }

  val
}

#' @rdname property
#' @export
property_safely <- function(object, name) {
  if (identical(name, ".data")) {
    # Remove properties, return the rest
    props <- properties(object)
    for (name in names(props)) {
      attr(object, name) <- NULL
    }
    class(object) <- setdiff(class_names(attr(object_class(object), "parent", exact = TRUE)), "R7_object")
    object_class(object) <- NULL
    return(object)
  }
  val <- attr(object, name, exact = TRUE)
  if (is.null(val)) {
    prop <- properties(object)[[name]]
    if (!is.null(prop$getter)) {
      val <- prop$getter(object)
    }
  }
  val
}

properties <- function(object) {
  obj_class <- object_class(object)
  prop <- list()
  while(!is.null(obj_class)) {
    prop <- c(attr(obj_class, "properties"), prop)
    obj_class <- attr(obj_class, "parent", exact = TRUE)
  }

  prop
}

#' @rdname property
#' @export
`property<-` <- function(object, name, value) {
  if (name == ".data") {
    attrs <- attributes(object)
    object <- value
    attributes(object) <- attrs
    validate(object)
    return(invisible(object))
  }

  prop <- properties(object)[[name]]
  if (!is.null(prop$setter)) {
    object <- prop$setter(object, value)
  } else {
    if (length(prop[["class"]]) > 0 &&
      !inherits(value, prop[["class"]])) {
      stop(sprintf("`value` must be of class <%s>:\n- `value` is of class <%s>", prop[["class"]][[1]], class(value)[[1]]), call. = FALSE)
    }
    attr(object, name) <- value
  }

  validate(object)

  invisible(object)
}

#' @rdname property
#' @usage object@name
#' @export
`@` <- function(object, name) {
  if (!inherits(object, "R7_object")) {
    if (is.null(object)) {
      return()
    }
    name <- substitute(name)
    return(do.call(base::`@`, list(object, name)))
  }

  nme <- as.character(substitute(name))
  property(object, nme)
}

#' @rawNamespace S3method("@<-",R7_object)
`@<-.R7_object` <- function(object, name, value) {
  if (!inherits(object, "R7_object")) {
    return(base::`@<-`(object, name))
  }

  nme <- as.character(substitute(name))
  property(object, nme) <- value

  invisible(object)
}

as_properties <- function(x) {
  if (length(x) == 0) {
    return(x)
  }

  named_chars <- vlapply(x, is.character) & has_names(x)
  R7_properties <- vlapply(x, inherits, "R7_property")

  if (!all(named_chars | R7_properties)) {
    stop("`x` must be a list of 'R7_property' objects or named characters", call. = FALSE)
  }

  x[named_chars] <- mapply(property_new, name = names(x)[named_chars], class = x[named_chars], USE.NAMES = TRUE, SIMPLIFY = FALSE)

  names(x)[!named_chars] <- vcapply(x[!named_chars], function(x) x[["name"]])

  x
}
