#' Define a new R7 class
#'
#' A class specifies the properties (data) that each of its objects will
#' possess. The class, and its parent, determines which method will be used
#' when an object is passed to a generic.
#'
#' @param name The name of the class, as a string.
#' @param parent The parent class to inherit behavior from.
#'   There are four options:
#'
#'   * The R7 class, like [R7_object].
#'   * An S3 class wrapped by [new_S3_class()].
#'   * A base type, like `logical`, `double`, or `character`.
#'
#' @param constructor The constructor function. Advanced use only.
#'
#'   A custom constructor should call `new_object()` to create the R7 object.
#'   The first argument, `.data`, should an instance of the parent class. The
#'   subsequent arguments are used to set the properties.
#' @param validator A function taking a single argument, `self`, the object
#'   to validate.
#'
#'   The job of a validator is to determine whether the object is valid,
#'   i.e. if the current property values form an allowed combination. The
#'   types of the properties are always automatically validated so the job of
#'   the validator is to verify that the value of individual properties is
#'   ok (i.e. maybe a property should have length 1, or should always be
#'   positive), or that the combination of values of multiple properties is ok.
#'   It is called after construction and whenever any property is set.
#'
#'   The validator should return `NULL` if the object is valid. If not, it
#'   should return a character vector where each element describes a single
#'   problem. It's generally helpful to report as many problems at once
#'   as possible.
#'
#'   See `validate()` for more details and examples.
#' @param properties A list specifying the properties (data) that
#'   every object of the class will possess. Each property can either be
#'   a named string (specifying the class), or a call to [new_property()],
#'   allowing greater flexibility.
#' @return A object constructor, a function that can be used to create objects
#'   of the given class.
#' @export
#' @examples
#' # Create an class that represents a range using a numeric start and end
#' range <- new_class("range",
#'   properties = list(
#'     start = "numeric",
#'     end = "numeric"
#'   )
#' )
#' r <- range(start = 10, end = 20)
#' r
#' # get and set properties with @
#' r@start
#' r@end <- 40
#' r@end
#'
#' # R7 automatically ensures that properties are of the declared types:
#' try(range(start = "hello", end = 20))
#'
#' # But we might also want to use a validator to ensure that start and end
#' # are length 1, and that start is < end
#' range <- new_class("range",
#'   properties = list(
#'     start = "numeric",
#'     end = "numeric"
#'   ),
#'   validator = function(self) {
#'     if (length(self@start) != 1) {
#'       "@start must be a single number"
#'     } else if (length(self@end) != 1) {
#'       "@end must be a single number"
#'     } else if (self@end < self@start) {
#'       "@end must be great than or equal to @start"
#'     }
#'   }
#' )
#' try(range(start = c(10, 15), end = 20))
#' try(range(start = 20, end = 10))
#'
#' r <- range(start = 10, end = 20)
#' try(r@start <- 25)
new_class <- function(
    name,
    parent = R7_object,
    properties = list(),
    constructor = NULL,
    validator = NULL) {

  check_name(name)

  parent <- as_class(parent)

  # Don't check arguments for R7_object
  if (!is.null(parent)) {
    check_can_inherit(parent)
    if (!is.null(constructor)) {
      check_R7_constructor(constructor)
    }
    if (!is.null(validator)) {
      check_function(validator, alist(self = ))
    }
  }

  # Combine properties from parent, overriding as needed
  all_props <- attr(parent, "properties", exact = TRUE) %||% list()
  new_props <- as_properties(properties)
  all_props[names(new_props)] <- new_props

  if (is.null(constructor)) {
    constructor <- new_constructor(parent, all_props)
  }

  object <- constructor
  # Must synchronise with prop_names
  attr(object, "name") <- name
  attr(object, "parent") <- parent
  attr(object, "properties") <- all_props
  attr(object, "constructor") <- constructor
  attr(object, "validator") <- validator
  class(object) <- c("R7_class", "R7_object")

  global_variables(names(all_props))
  object
}

check_R7_constructor <- function(constructor) {
  if (!is.function(constructor)) {
    stop("`constructor` must be a function", call. = FALSE)
  }

  method_call <- find_call(body(constructor), quote(new_object))
  if (is.null(method_call)) {
    stop("`constructor` must contain a call to `new_object()`", call. = FALSE)
  }
}

#' @export
print.R7_class <- function(x, ...) {
  props <- x@properties
  if (length(props) > 0) {
    prop_names <- format(names(props))
    prop_types <- format(vcapply(props, function(x) class_desc(x$class)))
    prop_fmt <- paste0(" $ ", prop_names, ": ", prop_types, "\n", collapse = "")
  } else {
    prop_fmt <- ""
  }

  cat(
    sprintf("<R7_class>\n@ name  :  %s\n@ parent: %s\n@ properties:\n%s",
      x@name,
      class_desc(x@parent),
      prop_fmt
    ),
    sep = ""
  )
  invisible(x)
}

#' @export
str.R7_class <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  cat("<", paste0(setdiff(class_dispatch(object), "ANY"), collapse = "/"), "> constructor", sep = "")
  cat("\n")

  if (nest.lev == 0) {
    str_nest(props(object), "@", ..., nest.lev = nest.lev)
  }
}

can_inherit <- function(x) is_base_class(x) || is_S3_class(x) || is_class(x)

check_can_inherit <- function(x, arg = deparse(substitute(x))) {
  if (!can_inherit(x)) {
    msg <- sprintf(
      "`%s` must be an R7 class, S3 class, or base type, not %s.",
      arg,
      class_friendly(x)
    )
    stop(msg, call. = FALSE)
  }
}

is_class <- function(x) inherits(x, "R7_class")

# Object ------------------------------------------------------------------

#' @param .parent,... Parent object and named properties used to construct the
#'   object.
#' @rdname new_class
#' @export
new_object <- function(.parent, ...) {
  class <- sys.function(-1)
  if (!inherits(class, "R7_class")) {
    stop("`new_object()` must be called from within a constructor")
  }

  args <- list(...)
  nms <- names(args)

  missing_props <- nms[vlapply(args, is_missing_class)]
  for(prop in missing_props) {
    args[[prop]] <- prop_default(class@properties[[prop]])
  }

  object <- .parent %||% class_construct(class@parent)
  attr(object, "object_class") <- class
  class(object) <- setdiff(class_dispatch(class), "ANY")

  for (nme in nms) {
    prop(object, nme, check = FALSE) <- args[[nme]]
  }
  validate(object)

  object
}

#' @export
print.R7_object <- function(x, ...) {
  str.R7_object(x, ...)
  invisible(x)
}
#' @export
str.R7_object <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  cat(obj_desc(object))

  if (typeof(object) != "S4") {
    bare <- object
    attributes(bare) <- NULL
    str(bare, nest.lev = nest.lev + 1)
  } else {
    cat("\n")
  }

  str_nest(props(object), "@", ..., nest.lev = nest.lev)
}

#' Retrieve the R7 class of an object
#' @param object The R7 object
#' @export
object_class <- function(object) {
  attr(object, "object_class", exact = TRUE)
}
