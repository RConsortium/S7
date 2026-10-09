Parent := new_class(properties = list(value = class_integer))
class_Parent <- Parent
Parent <- function() "ordinary function"
.onLoad <- function(...) S7_on_load()
