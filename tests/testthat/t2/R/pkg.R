R7::method_new("t1::foo", list("character", "numeric"), function(x, y) paste0("foo-", x, "-", y))

#' @importFrom t0 bar
R7::method_new("t0::bar", list("character", "numeric"), function(x, y) paste0("bar-", x, "-", y))

.onLoad <- function(libname, pkgname) {
  R7::method_register()
}
