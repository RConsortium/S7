foo <- R7::new_external_generic("t1", "foo")
R7::method(foo, "character") <- function(x) "foo"

bar <- R7::new_external_generic("t0", "bar")
R7::method(bar, "character") <- function(x) "bar"

.onLoad <- function(libname, pkgname) {
  R7::method_register()
}
