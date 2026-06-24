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
  package = NULL,
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
    stop2(msg)
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
    stop2(msg)
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

check_subsettable <- function(x, allow_env = FALSE, call = sys.call(-1L)) {
  allowed_types <- c(
    "list",
    "language",
    "pairlist",
    if (allow_env) "environment"
  )
  if (!typeof(x) %in% allowed_types) {
    stop2("S7 objects are not subsettable.", call = call)
  }
  invisible(TRUE)
}

S7_generic <- NULL

on_load_define_S7_generic <- function() {
  # we do this in .onLoad() because dynlib `prop_` symbol
  # is not available at pkg build time, and new_class()
  # errors if `@` is not usable.
  S7_generic <<- new_class(
    name = "S7_generic",
    package = NULL,
    properties = list(
      name = class_character,
      methods = class_environment,
      dispatch_args = class_character
    ),
    parent = class_function
  )
}

methods::setOldClass(c("S7_generic", "function", "S7_object"))
is_S7_generic <- function(x) inherits(x, "S7_generic")


S7_method <- NULL

on_load_define_S7_method <- function() {
  S7_method <<- new_class(
    "S7_method",
    package = NULL,
    parent = class_function,
    properties = list(generic = S7_generic, signature = class_list)
  )
}
methods::setOldClass(c("S7_method", "function", "S7_object"))

# trace() builds a "<class>WithTrace" S4 class for the traced function on
# the fly, but the machinery it uses assumes that class() returns a single
# string, which isn't true for S7 generics and methods. So we register the
# traceable classes in advance, along with an initialize method that works
# around the same assumption in methods:::.initTraceable() by giving it a
# classless copy of the function (#584).
make_traceable <- function(class, props) {
  slots <- rep(list("ANY"), length(props))
  names(slots) <- props

  methods::setClass(
    paste0(class, "WithTrace"),
    contains = c(class, "traceable"),
    slots = slots
  )
  methods::setMethod(
    "initialize",
    paste0(class, "WithTrace"),
    function(.Object, def, ...) {
      if (missing(def)) {
        return(methods::callNextMethod(.Object, ...))
      }
      original <- def
      class(def) <- NULL
      .Object <- methods::callNextMethod(.Object, def = def, ...)
      .Object@original <- original
      # Mirror the S7 properties so introspection (e.g. print methods that
      # access x@generic) keeps working on the traced object
      for (prop in props) {
        methods::slot(.Object, prop) <- attr(original, prop, exact = TRUE)
      }
      .Object
    }
  )
}
make_traceable("S7_generic", c("name", "methods", "dispatch_args"))
make_traceable("S7_method", c("generic", "signature"))

# hooks -------------------------------------------------------------------

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  if (getRversion() < "4.3.0") {
    env[[".conflicts.OK"]] <- TRUE
  }
}

.onLoad <- function(...) {
  activate_backward_compatiblility()

  on_load_define_environment()
  on_load_define_S7_generic()
  on_load_define_S7_method()
  on_load_make_convert_generic()
  on_load_define_ops()
  on_load_define_or_methods()
  on_load_define_S7_type()
  on_load_define_union_classes()
}
