#' Define a new property
#'
#' @param name The name of the property
#' @param class The class of the property
#' @param accessor The accessor use to retrieve the property (if any)
#' @export
property_new <- function(name, class = NULL, accessor = NULL) {
  if (!is.null(accessor) && is.null(class)) {
    class <- "function"
  }

  out <- list(name = name, class = class, accessor = accessor)
  class(out) <- "r7_property"

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
    stop(sprintf("`%s` objects do not have a `%s` property", class@name, name), call. = FALSE)
  }

  if (inherits(val, "r7_accessor")) {
    return(val(object))
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
    class(object) <- setdiff(class_names(property_safely(object_class(object), "parent")), "r7_object")
    object_class(object) <- NULL
    return(object)
  }
  val <- attr(object, name, exact = TRUE)
  if (is.null(val)) {
    return(NULL)
  }
  val
}

properties <- function(object) {
  obj_class <- object_class(object)
  prop <- list()
  while(!is.null(obj_class)) {
    prop <- c(obj_class@properties, prop)
    obj_class <- property_safely(obj_class, "parent")
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
  } else {
    prop <- properties(object)[[name]]
    if (!is.null(prop[["accessor"]])) {
      class(value) <- union("r7_accessor", class(value))
    } else {
      if (length(prop[["class"]]) > 0 &&
        !inherits(value, prop[["class"]])) {
        stop(sprintf("`value` must be of class '%s':\n- `value` is of class '%s'", prop[["class"]][[1]], class(value)[[1]]), call. = FALSE)
      }
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
  if (!inherits(object, "r7_object")) {
    name <- substitute(name)
    return(do.call(base::`@`, list(object, name)))
  }

  nme <- as.character(substitute(name))
  property(object, nme)
}

#' @rawNamespace S3method("@<-",r7_object)
`@<-.r7_object` <- function(object, name, value) {
  if (!inherits(object, "r7_object")) {
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
  r7_properties <- vlapply(x, inherits, "r7_property")

  if (!all(named_chars | r7_properties)) {
    stop("`x` must be a list of 'r7_property' objects or named characters", call. = FALSE)
  }

  x[named_chars] <- mapply(property_new, name = x[named_chars], class = x[named_chars], USE.NAMES = TRUE, SIMPLIFY = FALSE)

  names(x)[!named_chars] <- vcapply(x[!named_chars], function(x) x[["name"]])

  x
}
