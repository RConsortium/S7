Parent := new_class(
  properties = list(value = class_integer),
  constructor = function(value = 2L) {
    new_object(S7_object(), value = value + 10L)
  }
)
class_Parent <- Parent
Parent <- function() "ordinary function"
.onLoad <- function(...) S7_on_load()
