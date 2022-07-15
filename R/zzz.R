#' Base R7 class
#'
#' @keywords internal
#' @export
R7_object <- new_class(
  name = "R7_object",
  parent = NULL,
  constructor = function() {
    .Call(R7_object_)
  },
  validator = function(self) {
    if (typeof(self) != "S4") {
      "Underlying data is corrupt"
    }
  }
)
methods::setOldClass("R7_object")

#' @export
`$.R7_object` <- function(x, name) {
  if (typeof(x) %in% c("list", "environment")) {
    NextMethod()
  } else {
    msg <- sprintf(
      "Can't get R7 properties with `$`. Did you mean `%s@%s`?",
      deparse1(substitute(x)),
      name
    )
    stop(msg, call. = FALSE)
  }
}
#' @export
`$<-.R7_object` <- function(x, name, value) {
  if (typeof(x) %in% c("list", "environment")) {
    NextMethod()
  } else {
    msg <- sprintf(
      "Can't set R7 properties with `$`. Did you mean `...@%s <- %s`?",
      name,
      deparse1(substitute(value))
    )
    stop(msg, call. = FALSE)
  }
}

check_subsettable <- function(x, allow_env = FALSE) {
  # if not in the allowed types, throw the error
  allowed_types = c("list", "environment"[allow_env])
  if (! typeof(x) %in% allowed_types) {
    msg <- "R7 objects are not subsettable."
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' @export
`[.R7_object` <- function(x, ..., drop = TRUE) {
  check_subsettable(x)
  NextMethod()
}
#' @export
`[<-.R7_object` <- function(x, ..., value) {
  check_subsettable(x)
  NextMethod()
}

#' @export
`[[.R7_object` <- function(x, ...) {
  check_subsettable(x, allow_env = TRUE)
  NextMethod()
}
#' @export
`[[<-.R7_object` <- function(x, ..., value) {
  check_subsettable(x, allow_env = TRUE)
  NextMethod()
}

R7_generic <- new_class(
  name = "R7_generic",
  properties = list(
    name = class_character,
    methods = class_environment,
    dispatch_args = class_character
  ),
  parent = class_function
)
methods::setOldClass(c("R7_generic", "function", "R7_object"))
is_generic <- function(x) inherits(x, "R7_generic")

R7_method <- new_class("R7_method",
  parent = class_function,
  properties = list(
    generic = R7_generic,
    signature = class_list
  )
)
methods::setOldClass(c("R7_method", "function", "R7_object"))


# Create generics for double dispatch base Ops
base_ops <- lapply(setNames(, group_generics()$Ops), new_generic, dispatch_args = c("x", "y"))

#' @export
Ops.R7_object <- function(e1, e2) {
  base_ops[[.Generic]](e1, e2)
}


.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
}

.onLoad <- function(...) {
  convert <<- R7_generic(convert, name = "convert", dispatch_args = c("from", "to"))

  class_numeric <<- new_union(class_integer, class_double)
  class_atomic <<- new_union(class_logical, class_numeric, class_complex, class_character, class_raw)
  class_vector <<- new_union(class_atomic, class_expression, class_list)
}
