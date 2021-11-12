#' Initialize a new object
#' @keywords internal
#' @export
new_object <- function(.data = NULL, ...) {
  obj_cls <- object_class(sys.function(-1))

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
    # Verify .data satisfies the parent class
    cls_nms <- class_names(obj_cls@parent)

    if (!identical(cls_nms, "R7_object") && !inherits(.data, cls_nms)) {
      stop_bad_type(cls_nms, class(.data))
    }

    if (identical(cls_nms, "R7_object") && hasName(base_classes, obj_cls@name)) {
      if (!inherits(.data, obj_cls@name)) {
        stop_bad_type(obj_cls@name, class(.data))
      }
    }
    object <- .data
  } else {
    object <- obj_cls@parent@constructor()
  }
  attr(object, ".should_validate") <- FALSE

  class(object) <- "R7_object"
  object_class(object) <- obj_cls
  for (nme in nms) {
    prop(object, nme) <- args[[nme]]
  }

  attr(object, ".should_validate") <- NULL
  validate(object)

  object
}

stop_bad_type <- function(expected, actual) {
  stop(
    sprintf(
      "`.data` must be %s not %s",
      fmt_classes(expected),
      fmt_classes(actual[[1]])
    ),
    call. = FALSE
  )
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
  props <- props(x)
  if (length(props) > 0) {
    values <- lapply(names(props), function(xx) prop(x, xx))
    prop_names <- format(names(props))
    prop_values <- format(vcapply(names(props), function(name) paste0(format(prop(x, name)), collapse = "\n")), justify = "right")
    prop_fmt <- paste0(paste0("@", prop_names, " ", prop_values, collapse = "\n"), "\n")
  } else {
    prop_fmt <- ""
  }
  cat(sprintf("%s\n%s", fmt_classes(class(x), collapse = " "), prop_fmt), sep = "")
}

#' @export
str.R7_object <- function(object, ..., nest.lev = 0) {
  cat(" <", paste0(class(object), collapse = "/"), "> ", sep = "")

  if (typeof(object) != "S4") {
    bare <- unclass(object)
    attr(bare, "object_class") <- NULL
  } else {
    bare <- attributes(object)
    bare$class <- NULL
    bare$object_class <- NULL
  }
  str(bare, ..., nest.lev = nest.lev + 1)
}


