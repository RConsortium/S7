#' S7 wrappers for base condition classes
#'
#' @description
#' S7 bundles [S3 definitions][new_S3_class] for the base condition classes,
#' making it easier to use S7 to define custom condition classes:
#'
#' * `class_condition` for conditions, the parent class of all conditions.
#' * `class_error` for errors, as signalled by [stop()].
#' * `class_warning` for warnings, as signalled by [warning()].
#'
#' These classes are lists with `message` and `call` elements, since base
#' accessors like [conditionMessage()] and [conditionCall()] retrieve these
#' fields from the underlying list. Subclasses inherit these elements, so they
#' remain compatible with base condition handling.
#'
#' @name base_condition_classes
#' @order 0
#' @returns S7 classes wrapping around the base condition classes.
#' @examples
#' class_error
NULL

validate_condition <- function(self) {
  if (!is.list(self)) {
    return("Underlying data must be a <list>")
  }
  if (!is.character(self$message) || length(self$message) != 1) {
    return("`message` must be a single string")
  }
}

new_condition_constructor <- function(class) {
  function(.data = list(), message = "", call = NULL) {
    structure(
      c(list(message = message, call = call), .data),
      class = class
    )
  }
}

#' @export
#' @rdname base_condition_classes
#' @format NULL
#' @order 1
class_condition <- new_S3_class(
  "condition",
  constructor = new_condition_constructor("condition"),
  validator = validate_condition
)

#' @export
#' @rdname base_condition_classes
#' @format NULL
#' @order 1
class_error <- new_S3_class(
  c("error", "condition"),
  constructor = new_condition_constructor(c("error", "condition")),
  validator = validate_condition
)

#' @export
#' @rdname base_condition_classes
#' @format NULL
#' @order 1
class_warning <- new_S3_class(
  c("warning", "condition"),
  constructor = new_condition_constructor(c("warning", "condition")),
  validator = validate_condition
)
