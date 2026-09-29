Base := deprecated_class(
  properties = list(size = new_property(class_double, default = 7)),
  when = "2.0.0"
)
Warn := deprecated_class(
  properties = list(size = new_property(class_double, default = 7)),
  when = "2.0.0",
  method = "lifecycle(warn)"
)
Stop := deprecated_class(
  properties = list(size = new_property(class_double, default = 7)),
  when = "2.0.0",
  method = "lifecycle(stop)"
)
.onLoad <- function(...) S7_on_load()
