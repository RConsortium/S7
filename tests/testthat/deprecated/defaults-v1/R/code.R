Base := new_class(
  properties = list(size = new_property(class_double, default = 7))
)
Warn := new_class(
  properties = list(size = new_property(class_double, default = 7))
)
Stop := new_class(
  properties = list(size = new_property(class_double, default = 7))
)
.onLoad <- function(...) S7_on_load()
