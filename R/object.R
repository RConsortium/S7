#' Initialize a new object
#' @keywords internal
#' @export
new_object <- function(.data = NULL, ...) {
  class <- object_class(sys.function(-1))

  args <- list(...)
  nms <- names(args)

  if (!is.null(.data)) {
    object <- .data
  } else {
    object <- class@parent@constructor()
  }
  attr(object, ".should_validate") <- FALSE

  class(object) <- "R7_object"

  object_class(object) <- class

  props <- properties(object)

  to_set <- intersect(nms, names(props))

  for (nme in to_set) {
    property(object, nme) <- args[[nme]]
  }

  attr(object, ".should_validate") <- NULL

  validate(object)

  object
}

#' Retrieve the R7 class of an object
#' @param object The R7 object
#' @export
object_class <- function(object) {
  .Call(object_class_, object, parent.frame())
}

`object_class<-` <- function(object, value) {
  attr(object, "object_class") <- value

  nms <- class_names(object_class(object))

  class(object) <- nms

  invisible(object)
}

#' @export
print.R7_object <- function(x, ...) {
  props <- properties(x)
  if (length(props) > 0) {
    values <- lapply(names(props), function(xx) property(x, xx))
    prop_names <- format(names(props))
    prop_values <- format(vcapply(names(props), function(name) format(property(x, name))), justify = "right")
    prop_fmt <- paste0(paste0("@", prop_names, " ", prop_values, collapse = "\n"), "\n")
  } else {
    prop_fmt <- ""
  }
  cat(sprintf("<R7_object> <%s>\n%s", object_class(x)@name, prop_fmt), sep = "")
}
