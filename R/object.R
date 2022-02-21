#' @param .data,... Parent object and named properties used to construct the
#'   object.
#' @rdname new_class
#' @export
new_object <- function(.data, ...) {
  obj_cls <- sys.function(-1)
  if (!inherits(obj_cls, "R7_class")) {
    stop("`new_object()` must be called from within a constructor")
  }

  args <- list(...)
  nms <- names(args)
  if (length(args) > 0 && (is.null(nms) || any(nms == ""))) {
    stop(
      sprintf("All arguments to <%s> constructor must be named", obj_cls@name),
      call. = FALSE
    )
  }

  bad_names <- setdiff(nms, names(obj_cls@properties))
  if (length(bad_names) > 0) {
    stop(
      sprintf(
        "All arguments to <%s> constructor must be properties: %s",
        obj_cls@name,
        paste0(bad_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  missing_props <- nms[vlapply(args, is_missing_class)]
  for(prop in missing_props) {
    args[[prop]] <- prop_default(obj_cls@properties[[prop]])
  }

  if (!is.null(.data)) {
    object <- .data
  } else {
    object <- class_construct(obj_cls@parent)
  }

  attr(object, "object_class") <- obj_cls
  class(object) <- setdiff(class_dispatch(obj_cls), "ANY")

  for (nme in nms) {
    prop(object, nme, check = FALSE) <- args[[nme]]
  }
  validate(object)

  object
}

#' Retrieve the R7 class of an object
#' @param object The R7 object
#' @export
object_class <- function(object) {
  attr(object, "object_class", exact = TRUE)
}

#' @export
print.R7_object <- function(x, ...) {
  str.R7_object(x, ...)
  invisible(x)
}
#' @export
str.R7_object <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  cat(obj_desc(object))

  if (typeof(object) != "S4") {
    bare <- object
    attributes(bare) <- NULL
    str(bare, nest.lev = nest.lev + 1)
  } else {
    cat("\n")
  }

  str_nest(props(object), "@", ..., nest.lev = nest.lev)
}
