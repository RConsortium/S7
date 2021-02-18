object_new <- function(...) {
  class <- object_class(sys.function(-1))
  obj <- class@parent@constructor()
  object_class(obj) <- class

  args <- list(...)
  nms <- names(args)
  for (i in seq_along(args)) {
    attr(obj, nms[[i]]) <- args[[i]]
  }

  validate(obj)

  obj
}

object_class <- function(obj) {
  if (inherits(obj, "r7_class")) {
    return(obj)
  }

  attr(obj, "object_class", exact = TRUE)
}

`object_class<-` <- function(obj, value) {
  attr(obj, "object_class") <- value

  nms <- class_names(obj)

  class(obj) <- nms


  invisible(obj)
}

class_names <- function(obj) {
  obj_class <- object_class(obj)

  nms <- prop(obj_class, "name")
  parent <- prop(obj_class, "parent")
  while(!is.null(parent)) {
    nms <- c(nms, prop(parent, "name"))
    parent <- prop(parent, "parent")
  }
  nms
}
