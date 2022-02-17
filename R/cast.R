up_cast <- function(object, class) {
  check_R7(object)

  class <- as_class(class)
  check_can_inherit(class)
  if (!class_inherits(object, class)) {
    msg <- sprintf("`object` %s does not inherit from %s", obj_desc(object), class_desc(class))
    stop(msg)
  }

  structure(
    list(
      object = object,
      class = class
    ),
    class = "R7_upclass"
  )
}

is_up_cast <- function(x) inherits(x, "R7_upclass")
