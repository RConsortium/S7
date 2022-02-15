#' Initialize a new object
#' @keywords internal
#' @export
new_object <- function(.data = NULL, ...) {
  obj_cls <- sys.function(-1)

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

  if (!is.null(.data)) {
    object <- .data
  } else {
    object <- class_construct(obj_cls@parent)
  }

  class(object) <- "R7_object"
  object_class(object) <- obj_cls
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

`object_class<-` <- function(object, value) {
  if (!inherits(value, "R7_class")) {
    stop("`value` must be an <R7_class>")
  }

  attr(object, "object_class") <- value
  class(object) <- class_dispatch(value)

  invisible(object)
}

#' @export
print.R7_object <- function(x, ...) {
  str.R7_object(x)
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

  str_list(props(object), ..., nest.lev = nest.lev, prefix = "@")
}

str_list <- function(
    object,
    ...,
    nest.lev = 0,
    indent.str = paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."),
    prefix = "$"
) {

  names <- format(names(object))

  for (i in seq_along(object)) {
    cat(if (nest.lev > 0) indent.str, prefix, " ", names[[i]], ": ", sep = "")

    xi <- object[[i]]
    if (is.function(xi)) {
      str_function(xi, nest.lev = nest.lev + 1)
    } else {
      str(xi, ..., nest.lev = nest.lev + 1)
    }
  }
}

str_function <- function(object, ..., nest.lev = 0) {
  attr(object, "srcref") <- NULL
  if (identical(class(object), "function")) {
    cat(" ")
  }
  str(object, ..., nest.lev = nest.lev)
}
