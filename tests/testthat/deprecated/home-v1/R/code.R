gen := new_generic("x")
Foo := new_class(properties = list(value = class_double))
.onLoad <- function(...) S7_on_load()
