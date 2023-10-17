# Where needed, attach an environment containing @ that works with S7
activate_backward_compatiblility <- function() {
  if (getRversion() < "4.3.0" && !"S7_at" %in% search()) {
    args <- list(list("@" = `@`), name = "S7_at", warn.conflicts = FALSE)
    do.call("attach", args)
  }
  invisible()
}

#' @aliases @
#' @usage NULL
#' @rawNamespace if (getRversion() < "4.3.0") export(`@`)
#' @name prop
`@` <- function(object, name) {
  if (inherits(object, "S7_object")) {
    name <- as.character(substitute(name))
    prop(object, name)
  } else {
    name <- substitute(name)
    do.call(base::`@`, list(object, name))
  }
}
