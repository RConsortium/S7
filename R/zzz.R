global_variables(c("name", "parent", "properties", "constructor", "validator"))

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
  validator = function(object) {
    if (typeof(object) != "S4") {
      "Underlying data is corrupt"
    }
  }
)
check_R7 <- function(x, arg = deparse(substitute(x))) {
  if (!inherits(x, "R7_object")) {
    stop(sprintf("`%s` is not an <R7_object>", arg), call. = FALSE)
  }
}

R7_generic <- new_class(
  name = "R7_generic",
  properties = list(
    name = "character",
    methods = "environment",
    dispatch_args = new_property(
      name = "dispatch_args",
      getter = function(x) formals(R7_data(x))
  )),
  parent = "function",
  constructor = function(name, dispatch_args, fun) {
    new_object(
      name = name,
      dispatch_args = dispatch_args,
      methods = new.env(parent = emptyenv(), hash = TRUE),
      .data = fun
    )
  }
)

R7_method <- new_class("R7_method",
  parent = "function",
  properties = list(
    generic = R7_generic,
    signature = "list"
  )
)

.onAttach <- function(libname, pkgname) {
  env <- as.environment(paste0("package:", pkgname))
  env[[".conflicts.OK"]] <- TRUE
}

.onLoad <- function(...) {
  base_unions$numeric <<- new_union("integer", "double")
  base_unions$atomic <<- new_union("logical", "integer", "double", "complex", "character", "raw")
  base_unions$vector <<- new_union("logical", "integer", "double", "complex", "character", "raw", "expression", "list")
}
