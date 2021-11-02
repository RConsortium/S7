#' Validation of R7 objects
#'
#' [validate()] calls the validation of an R7 object. This is done
#' automatically when creating new objects (at the end of [new_object]) and
#' when setting any property.
#'
#' [valid_eventually()] disables validation of properties, runs a function on
#' the object, then validates the object.
#'
#' [valid_implicitly()] does the same but does not validate the object at the end.
#'
#' [valid_implicitly()] should only be used rarely in performance critical code
#' where you are certain a sequence of operations cannot produce an invalid
#' object.
#' @param object An R7 object
#' @param fun A function to call on the object before validation.
validate <- function(object) {
  if (!is.null(attr(object, ".should_validate"))) {
    return(invisible(object))
  }

  obj_class <- object_class(object)

  validator <- prop_safely(obj_class, "validator")
  if (is.null(validator)) {
    return(invisible(object))
  }

  errors <- validator(object)

  if (length(errors) > 0) {
    msg <- sprintf("Invalid <%s> object:\n%s", obj_class@name, paste0("- ", errors, collapse = "\n"))
    stop(msg, call. = FALSE)
  }

  invisible(object)
}

#' @rdname validate
#' @export
valid_eventually <- function(object, fun) {
  old <- attr(object, ".should_validate")
  attr(object, ".should_validate") <- FALSE
  out <- fun(object)
  attr(out, ".should_validate") <- old
  validate(out)
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
