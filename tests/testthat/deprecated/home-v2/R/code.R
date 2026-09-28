gen := deprecated_generic(new = deprecatedCore::gen, when = "2.0.0")
Foo := new_class(properties = list(value = class_double))
Bar <- Foo
Foo := deprecated_class(new = Bar, when = "2.0.0", new_label = "Bar()")
.onLoad <- function(...) S7_on_load()
