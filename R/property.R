property_new <- function(name, class = NULL, accessor = NULL) {
  out <- list(name = name, class = class, accessor = accessor)
  class(out) <- "r7_property"

  out
}

#' Extract or replace a property
#'
#' - [property] or the shorthand `@` extracts a given property, throwing an error if the property doesn't exist for that object.
#' - [property_safely] returns `NULL` if a property doesn't exist, rather than throwing an error.
#' - [property<-] assigns a new value for a given property.
#' @param obj An object from a R7 class
#' @param name The name of the parameter as a character. No partial matching is done.
#' @param value A replacement value for the parameter. The object is
#'   automatically checked for validity after the replacement is done.
#' @export
property <- function(obj, name) {
  val <- property_safely(obj, name)
  if (is.null(val)) {
    class <- object_class(obj)
    stop(sprintf("`%s` objects do not have a `%s` property", class@name, name), call. = FALSE)
  }

  if (inherits(val, "r7_accessor")) {
    return(val(obj))
  }

  val
}

#' @rdname property
#' @export
property_safely <- function(obj, name) {
  if (identical(name, ".data")) {
    # Remove properties, return the rest
    props <- properties(obj)
    for (name in names(props)) {
      attr(obj, name) <- NULL
    }
    class(obj) <- setdiff(class_names(property_safely(object_class(obj), "parent")), "r7_object")
    object_class(obj) <- NULL
    return(obj)
  }
  val <- attr(obj, name, exact = TRUE)
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
`property<-` <- function(obj, name, value) {
  if (name == ".data") {
    attrs <- attributes(obj)
    obj <- value
    attributes(obj) <- attrs
  } else {
    prop <- properties(obj)[[name]]
    if (!is.null(prop[["accessor"]])) {
      class(value) <- union("r7_accessor", class(value))
    } else {
      if (length(prop[["class"]]) > 0 &&
        !inherits(value, prop[["class"]])) {
        stop(sprintf("`value` must be of class '%s':\n- `value` is of class '%s'", prop[["class"]][[1]], class(value)[[1]]), call. = FALSE)
      }
    }

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
    return(do.call(base::`@`, list(obj, name)))
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
