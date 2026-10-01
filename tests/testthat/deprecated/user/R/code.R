method(gen, class_double) <- function(x, ...) "double"
method(shout, class_character) <- \(x, ...) toupper(x)
method(gen, Foo) <- function(x, ...) x@value
Child := new_class(parent = Foo)
saved <- Child(value = 3)
gen := new_external_generic(package = "deprecatedHome", dispatch_args = "x")
method(gen, class_character) <- function(x, ...) "character"
.onLoad <- function(...) S7_on_load()
