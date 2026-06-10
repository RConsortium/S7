# `ChildClass` subclasses `ParentClass`, which is defined in t1 (a soft
# dependency). Because the parent is referenced via new_external_class(), t5 can
# be built and loaded even when t1 is not installed: the external class is only
# resolved when an object is actually constructed.

#' @export
ChildClass <- S7::new_class(
  "ChildClass",
  package = "t5",
  parent = S7::new_external_class("t1", "ParentClass"),
  properties = list(child_prop = S7::class_character)
)

.onLoad <- function(libname, pkgname) {
  S7::S7_on_load()
}
