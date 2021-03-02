#' Initialize a new object
#' @keywords internal
#' @export
object_new <- function(...) {
  class <- object_class(sys.function(-1))

  args <- list(...)
  nms <- names(args)

  if (".data" %in% nms) {
    obj <- args[[".data"]]
  } else {
    obj <- class@parent@constructor()
  }
  attr(obj, ".should_validate") <- FALSE

  class(obj) <- "r7_object"

  object_class(obj) <- class

  props <- properties(obj)

  to_set <- intersect(nms, names(props))

  for (nme in to_set) {
    property(obj, nme) <- args[[nme]]
  }

  attr(obj, ".should_validate") <- NULL

  validate(obj)

  obj
}

#' Retrieve the r7 class of an object
#' @param obj The r7 object
#' @export
object_class <- function(obj) {
  if (inherits(obj, "r7_class")) {
    return(obj)
  }
  if (inherits(obj, "r7_object")) {
    return(attr(obj, "object_class"))
  }
  if (isS4(obj)) {
    return(methods::extends(class(obj)))
  }

  class(obj)
}

`object_class<-` <- function(obj, value) {
  attr(obj, "object_class") <- value

  nms <- class_names(object_class(obj))

  class(obj) <- nms

  invisible(obj)
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
