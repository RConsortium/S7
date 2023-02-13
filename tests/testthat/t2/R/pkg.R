foo <- S7::new_external_generic("t1", "foo", "x")
S7::method(foo, S7::class_character) <- function(x) "foo"

bar <- S7::new_external_generic("t0", "bar", "x")
S7::method(bar, S7::class_character) <- function(x) "bar"

.onLoad <- function(libname, pkgname) {
  S7::external_methods_register()
}
