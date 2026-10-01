gen := deprecated_generic(new = deprecatedCore::gen, when = "2.0.0")
shout := deprecated_generic("x", when = "2.0.0")
Bar := new_class(properties = list(value = class_double))
Foo := deprecated_class(
  properties = list(value = class_double),
  replacement = Bar,
  when = "2.0.0"
)
.onLoad <- function(...) S7_on_load()
