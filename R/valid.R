#' Validate an S7 object
#'
#' @description
#' `validate()` ensures that an S7 object is valid by calling the `validator`
#' provided in [new_class()]. This is done automatically when constructing new
#' objects and when modifying properties.
#'
#' `valid_eventually()` disables validation, modifies the object, then
#' revalidates. This is useful when a sequence of operations would otherwise
#' lead an object to be temporarily invalid, or when repeated property
#' modification causes a performance bottleneck because the validator is
#' relatively expensive.
#'
#' `valid_implicitly()` does the same but does not validate the object at the
#' end. It should only be used rarely, and in performance critical code where
#' you are certain a sequence of operations cannot produce an invalid object.
#' @param object An S7 object
#' @param fun A function to call on the object before validation.
#' @param recursive If `TRUE`, calls validator of parent classes recursively.
#' @param properties If `TRUE`, the default, checks property types before
#'   executing the validator.
#' @returns Either `object` invisibly if valid, otherwise an error of
#'   class `S7_error_validation_failed`
#' @export
#' @examples
#' # A range class might validate that the start is less than the end
#' Range := new_class(
#'   properties = list(start = class_double, end = class_double),
#'   validator = function(self) {
#'     if (self@start >= self@end) "start must be smaller than end"
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
#'
#' # Alternatively, you can set multiple properties at once using props<-,
#' # which validates once at the end
#' rightwards <- function(r, x) {
#'   props(r) <- list(start = r@start + x, end = r@end + x)
#'   r
#' }
#' rightwards(r, 20)
validate <- function(object, recursive = TRUE, properties = TRUE) {
  check_is_S7(object)

  parent <- validate_parent(object, recursive)
  validate_from(
    object,
    parent = parent,
    properties = properties,
    call = sys.call()
  )
}

validate_parent <- function(object, recursive) {
  if (!recursive) {
    return(S7_class(object)@parent)
  }

  if (
    isS4(object) && has_S7_class(object) && !S7_extends_S4(S7_class(object))
  ) {
    return(S7_object)
  }

  NULL
}

# validates `object` assuming `parent` (if supplied) has been validated
validate_from <- function(
  object,
  parent = NULL,
  properties = TRUE,
  call = sys.call(-1L)
) {
  if (!is.null(attr(object, ".should_validate"))) {
    return(invisible(object))
  }

  class <- S7_class(object)

  # First, check property types - if these are incorrect, the validator
  # is likely to return spurious errors
  if (properties) {
    errors <- validate_properties(object, class, parent_class = parent)
    if (length(errors) > 0) {
      bullets <- paste0("- ", errors, collapse = "\n")
      msg <- sprintf(
        "%s object properties are invalid:\n%s",
        obj_desc(object),
        bullets
      )
      stop2(msg, call = call, class = "S7_error_validation_failed")
    }
  }

  # Next, walk up the class hierarchy and run validators, stopping at `parent`
  errors <- character()
  repeat {
    error <- class_validate(class, object)
    if (is.null(error)) {} else if (is.character(error)) {
      append(errors) <- error
    } else {
      stop2(
        sprintf(
          "%s validator must return NULL or a character, not <%s>.",
          obj_desc(class),
          typeof(error)
        ),
        call = call
      )
    }
    if (!is_class(class) || identical(class@parent, parent)) {
      break
    }
    class <- class@parent
  }

  errors <- c(errors, validate_S4_subclass(object))

  # If needed, report errors
  if (length(errors) > 0) {
    bullets <- paste0("- ", errors, collapse = "\n")
    msg <- sprintf("%s object is invalid:\n%s", obj_desc(object), bullets)
    stop2(msg, call = call, class = "S7_error_validation_failed")
  }

  invisible(object)
}

validate_S4_subclass <- function(object) {
  if (!isS4(object) || !has_S7_class(object)) {
    return(NULL)
  }

  class <- methods::getClass(class(object)[[1L]])
  if (S4_is_reified_S7_class(class)) {
    return(NULL)
  }

  ancestor <- S4_ancestor(S7_class(object))
  skip <- if (is.null(ancestor)) character() else S4_validity_classes(ancestor)
  S4_validate_old_class(class, object, skip = skip)
}

S4_validity_classes <- function(class) {
  c(vcapply(class@contains, slot, "superClass"), as.character(class@className))
}

validate_properties <- function(object, class, parent_class = NULL) {
  errors <- character()
  parent_props <- if (is_class(parent_class)) parent_class@properties

  for (prop_obj in class@properties) {
    # Don't validate dynamic properties
    if (!is.null(prop_obj$getter)) {
      next
    }
    # Skip properties inherited unchanged from an already-validated parent
    if (identical(parent_props[[prop_obj$name]], prop_obj)) {
      next
    }

    value <- prop(object, prop_obj$name)
    errors <- c(errors, prop_validate(prop_obj, value))
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

  out
}
