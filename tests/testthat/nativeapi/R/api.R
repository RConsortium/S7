.onUnload <- function(libpath) {
  library.dynam.unload("nativeapi", libpath)
}

get_class <- function(object, interface = c("c", "cpp", "registered")) {
  routine <- switch(
    match.arg(interface),
    c = native_class,
    cpp = cpp_class,
    registered = registered_class
  )
  .Call(routine, object)
}

get_prop <- function(object, name, interface = c("c", "cpp", "registered")) {
  routine <- switch(
    match.arg(interface),
    c = native_prop,
    cpp = cpp_prop,
    registered = registered_prop
  )
  .Call(routine, object, name)
}
