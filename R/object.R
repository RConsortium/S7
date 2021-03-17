#' Initialize a new object
#' @keywords internal
#' @export
new_object <- function(.data = NULL, ...) {
  obj_cls <- object_class(sys.function(-1))

  args <- list(...)
  nms <- names(args)


  if (!is.null(.data)) {
    # Verify .data satisfies the parent class
    cls_nms <- class_names(obj_cls@parent)
    if (!identical(cls_nms, "R7_object") && !inherits(.data, cls_nms)) {
      stop(sprintf("`.data` must be a %s\n-`.data` is of type %s", fmt_classes(cls_nms), fmt_classes(class(.data)[[1]])), call. = FALSE)
    }
    object <- .data
  } else {
    object <- obj_cls@parent@constructor()
  }
  attr(object, ".should_validate") <- FALSE

  class(object) <- "R7_object"

  object_class(object) <- obj_cls

  props <- properties(object)

  to_set <- intersect(nms, names(props))
  # TODO: error if not all arguments are names of properties, likely a typo

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
    prop_values <- format(vcapply(names(props), function(name) paste0(format(property(x, name)), collapse = "\n")), justify = "right")
    prop_fmt <- paste0(paste0("@", prop_names, " ", prop_values, collapse = "\n"), "\n")
  } else {
    prop_fmt <- ""
  }
  cat(sprintf("%s\n%s", fmt_classes(class(x), collapse = " "), prop_fmt), sep = "")
}
