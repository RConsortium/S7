#' @export
up_cast <- function(object, class) {
  check_R7(object)

  class <- as_class(class)
  check_can_inherit(class)
  if (!class_inherits(object, class)) {
    msg <- sprintf("`object` %s does not inherit from %s", obj_desc(object), class_desc(class))
    stop(msg)
  }

  # Must not change order of these fields as C code indexes by position
  structure(
    list(
      object = object,
      dispatch = class_register(class)
    ),
    class = "R7_upclass"
  )
}

is_up_cast <- function(x) inherits(x, "R7_upclass")
