#' Validate an R7 object
#'
#' @description
#' `validate()` calls the validator of an R7 object. This is done automatically
#' when creating new objects (at the end of [new_object()]) and when setting
#' any property with [prop<-].
#'
#' `valid_eventually()` disables validation, modifies the object, then
#' revalidates. This is useful when a sequence of operations would otherwise
#' lead an object to be temporarily invalid.
#'
#' `valid_implicitly()` does the same but does not validate the object at the
#' end. It should only be used rarely, and in performance critical code where
#' you are certain a sequence of operations cannot produce an invalid object.
#' @param object An R7 object
#' @param fun A function to call on the object before validation.
#' @param properties If `TRUE`, the default, checks property types before
#'   executing the validator.
#' @export
#' @examples
#' # A range class might validate that the start is less than the end
#' Range <- new_class("Range",
#'   properties = list(start = "double", end = "double"),
#'   validator = function(object) {
#'     if (object@start >= object@end) "start must be smaller than end"
#'   }
#' )
#' # You can't construct an invalid object:
#' try(Range(1, 1))
#'
#' # And you can't create an invalid object with @<-
#' r <- Range(1, 2)
#' try(r@end <- 1)
#'
#' # But what if you want to move a range to the right?
#' rightwards <- function(r, x) {
#'   r@start <- r@start + x
#'   r@end <- r@end + x
#'   r
#' }
#' # This function doesn't work because it creates a temporarily invalid state
#' try(rightwards(r, 10))
#'
#' # This is the perfect use case for valid_eventually():
#' rightwards <- function(r, x) {
#'   valid_eventually(r, function(object) {
#'     object@start <- object@start + x
#'     object@end <- object@end + x
#'     object
#'   })
#' }
#' rightwards(r, 10)
validate <- function(object, properties = TRUE) {
  if (!is.null(attr(object, ".should_validate"))) {
    return(invisible(object))
  }

  class <- object_class(object)

  # First, check property types - if these are incorrect, the validator
  # is likely to return spurious errors
  if (properties) {
    errors <- validate_properties(object, class)
  } else {
    errors <- character()
  }

  # Next, recursively validate the object
  if (length(errors) == 0) {
    while(!is.null(class) && is_class(class)) {
      errors <- c(errors, class@validator(object))
      class <- prop_safely(class, "parent")
    }
  }

  # If needed, report errors
  if (length(errors) > 0) {
    bullets <- paste0("- ", errors, collapse = "\n")
    msg <- sprintf("Invalid %s object:\n%s", obj_desc(object), bullets)
    stop(msg, call. = FALSE)
  }

  invisible(object)
}

validate_properties <- function(object, class) {
  errors <- character()

  for (prop in class@properties) {
    # Only validate static properties
    if (!is.null(prop$getter) || !is.null(prop$setter)) {
      next
    }

    value <- prop(object, prop$name)
    if (!class_inherits(value, prop$class)) {
      errors <- c(errors, prop_error_type(object, prop$name, prop$class, value))
    }
    prop
  }

  errors
}

#' @rdname validate
#' @export
valid_eventually <- function(object, fun) {
  old <- attr(object, ".should_validate")
  attr(object, ".should_validate") <- FALSE
  out <- fun(object)
  attr(out, ".should_validate") <- old
  validate(out)

  out
}

#' @rdname validate
#' @export
valid_implicitly <- function(object, fun) {
  old <- attr(object, ".should_validate")
  attr(object, ".should_validate") <- FALSE
  out <- fun(object)
  attr(out, ".should_validate") <- old
  invisible(out)
}
