#' Initialize a new object
#' @keywords internal
#' @export
object_new <- function(...) {
  class <- object_class(sys.function(-1))

  args <- list(...)
  nms <- names(args)

  if (".data" %in% nms) {
    object <- args[[".data"]]
  } else {
    object <- class@parent@constructor()
  }
  attr(object, ".should_validate") <- FALSE

  class(object) <- "r7_object"

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

#' Retrieve the r7 class of an object
#' @param object The r7 object
#' @export
object_class <- function(object) {
  if (inherits(object, "r7_class")) {
    return(object)
  }
  if (inherits(object, "r7_object")) {
    return(attr(object, "object_class"))
  }
  if (isS4(object)) {
    return(methods::extends(class(object)))
  }

  class(object)
}

`object_class<-` <- function(object, value) {
  attr(object, "object_class") <- value

  nms <- class_names(object_class(object))

  class(object) <- nms

  invisible(object)
}

#' @export
print.r7_object <- function(x, ...) {
  props <- properties(x)
  if (length(props) > 0) {
    values <- lapply(names(props), function(xx) property(x, xx))
    prop_names <- format(paste0(names(props), ": "))
    prop_values <- format(vcapply(names(props), function(name) format(property(x, name))), justify = "right")
    prop_fmt <- paste0(paste0("| ", prop_names, prop_values, collapse = "\n"), "\n")
  } else {
    prop_fmt <- ""
  }
  cat(sprintf("r7: <%s>\n%s", object_class(x)@name, prop_fmt), sep = "")
}

#' @export
print.r7_class <- function(x, ...) {
  props <- properties(x)
  if (length(props) > 0) {
    prop_names <- format(paste0(names(props), ": "))
    prop_types <- format(paste0("<", vcapply(props, function(xx) xx[["class"]] %||% ""), ">"), justify = "right")
    prop_fmt <- paste0(paste0("| ", prop_names, prop_types, collapse = "\n"), "\n")
  } else {
    prop_fmt <- ""
  }
  cat(sprintf("r7: <%s>\n%s", object_class(x)@name, prop_fmt), sep = "")
}
