#' Define a new generic
#' @inheritParams r7_generic
#' @export
generic_new <- function(name, signature) {
  r7_generic(name = name, signature, envir = parent.frame())
}
