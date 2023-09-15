#' Base S7 class
#'
#' The base class from which all S7 classes eventually inherit from.
#'
#' @keywords internal
#' @export
#' @return The base S7 object.
#' @examples
#'
#' S7_object
S7_object <- new_class(
  name = "S7_object",
  parent = NULL,
  constructor = function() {
    .Call(S7_object_)
  },
  validator = function(self) {
    if (!is_S7_type(self)) {
      "Underlying data is corrupt"
    }
  }
)
methods::setOldClass("S7_object")

.S7_type <- NULL
# Defined onLoad because it depends on R version
on_load_define_S7_type <- function() {
  .S7_type <<- typeof(.Call(S7_object_))
}
is_S7_type <- function(x) {
  typeof(x) == .S7_type
}

#' @export
`$.S7_object` <- function(x, name) {
  if (typeof(x) %in% c("list", "environment")) {
    NextMethod()
  } else {
    msg <- sprintf(
      "Can't get S7 properties with `$`. Did you mean `%s@%s`?",
      deparse1(substitute(x)),
      name
    )
    stop(msg, call. = FALSE)
  }
}
#' @export
`$<-.S7_object` <- function(x, name, value) {
  if (typeof(x) %in% c("list", "environment")) {
    NextMethod()
  } else {
    msg <- sprintf(
      "Can't set S7 properties with `$`. Did you mean `...@%s <- %s`?",
      name,
      deparse1(substitute(value))
    )
    stop(msg, call. = FALSE)
  }
}

#' @export
`[.S7_object` <- function(x, ..., drop = TRUE) {
  check_subsettable(x)
  NextMethod()
}
#' @export
`[<-.S7_object` <- function(x, ..., value) {
  check_subsettable(x)
  NextMethod()
}

#' @export
`[[.S7_object` <- function(x, ...) {
  check_subsettable(x, allow_env = TRUE)
  NextMethod()
}
#' @export
`[[<-.S7_object` <- function(x, ..., value) {
  check_subsettable(x, allow_env = TRUE)
  NextMethod()
}

check_subsettable <- function(x, allow_env = FALSE) {
  allowed_types <- c("list", if (allow_env) "environment")
  if (!typeof(x) %in% allowed_types) {
    stop("S7 objects are not subsettable.", call. = FALSE)
  }
  invisible(TRUE)
}

S7_generic <- new_class(
  name = "S7_generic",
  properties = list(
    name = class_character,
    methods = class_environment,
    dispatch_args = class_character
  ),
  parent = class_function
)
methods::setOldClass(c("S7_generic", "function", "S7_object"))
is_generic <- function(x) inherits(x, "S7_generic")

S7_method <- new_class("S7_method",
  parent = class_function,
  properties = list(
    generic = S7_generic,
    signature = class_list
  )
)
methods::setOldClass(c("S7_method", "function", "S7_object"))

# hooks -------------------------------------------------------------------

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  if (getRversion() < "4.3.0") {
    env[[".conflicts.OK"]] <- TRUE
  }
}

.onLoad <- function(...) {
  on_load_make_convert_generic()
  on_load_define_matrixOps()
  on_load_define_ops()
  on_load_define_or_methods()
  on_load_define_S7_type()
  on_load_define_union_classes()
}
