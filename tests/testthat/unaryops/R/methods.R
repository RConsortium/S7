Number := new_class(parent = class_double)
Flag := new_class(parent = class_logical)

method(`+`, list(Number, class_missing)) <- \(e1, e2) "plus"
method(`-`, list(Number, class_missing)) <- \(e1, e2) "minus"
method(`!`, Flag) <- \(e1) "not"

.onLoad <- function(...) S7_on_load()
.onUnload <- function(...) S7_on_unload()
S7_on_build()
