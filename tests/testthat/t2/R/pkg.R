foo <- R7::new_external_generic("t1", "foo", "x")
R7::method(foo, R7::class_character) <- function(x) "foo"

bar <- R7::new_external_generic("t0", "bar", "x")
R7::method(bar, R7::class_character) <- function(x) "bar"

.onLoad <- function(libname, pkgname) {
  R7::external_methods_register()
}
