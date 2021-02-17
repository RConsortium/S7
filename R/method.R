`method` <- function(generic, class) {
  getS3method(generic, class)
}

method_register <- function(generic, class, value) {
  registerS3method(generic, class, value, envir = parent.frame())
}
