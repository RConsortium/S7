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
    if (typeof(self) != .S7_type) {
      "Underlying data is corrupt"
    }
  }
)
methods::setOldClass("S7_object")

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


# Create generics for double dispatch base Ops
base_ops <- lapply(setNames(, unlist(group_generics()[c("Ops", "matrixOps")])),
                   new_generic, dispatch_args = c("x", "y"))

#' @export
Ops.S7_object <- function(e1, e2) {
  base_ops[[.Generic]](e1, e2)
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(matrixOps, S7_object)
matrixOps.S7_object <- Ops.S7_object

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod, S7_object)
chooseOpsMethod.S7_object <- function(x, y, mx, my, cl, reverse) TRUE

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  if (getRversion() < "4.3.0") {
    env[[".conflicts.OK"]] <- TRUE
  }
}

.onLoad <- function(...) {
  ## "S4"   or [in R-devel 2023-07-x]   "object"
  assign(".S7_type", typeof(.Call(S7_object_)), topenv())

  convert <<- S7_generic(convert, name = "convert", dispatch_args = c("from", "to"))

  class_numeric <<- new_union(class_integer, class_double)
  class_atomic <<- new_union(class_logical, class_numeric, class_complex, class_character, class_raw)
  class_vector <<- new_union(class_atomic, class_expression, class_list)

  # Dynamic register so that function pointers are the same, avoiding R 4.0
  # and earlier bug related to incompatible S3 methods during Ops dispatch
  registerS3method("|", "S7_union", `|.S7_class`)
  registerS3method("|", "S7_base_class", `|.S7_class`)
  registerS3method("|", "S7_S3_class", `|.S7_class`)
  registerS3method("|", "classGeneratorFunction", `|.S7_class`)
  registerS3method("|", "ClassUnionRepresentation", `|.S7_class`)
  registerS3method("|", "classRepresentation", `|.S7_class`)
}
