method_exists <- function(object, name) {
  if (!inherits(object, "R7_object")) return(FALSE)
  name %in% names(attr(R7_class(object), "methods"))
}

method_val <- function(object, name) {
  class <- R7_class(object)
  method <- class@methods[[name]]

  # TODO: clone arguments
  # TODO: think about performance?
  function(...) {
    method(object, ...)
  }
}
