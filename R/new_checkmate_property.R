#' Create a new property that is automatically validated by checkmate
#'
#' @description
#' Define a property that has a `checkmate::check` function associated. This
#' makes it simple to define a property with specific expectations that
#' are validated at the time of object creation.
#'
#'
#' @param class Class that the property must be an instance of.
#'   See [as_class()] for details.
#' @param getter An optional function used to get the value. The function
#'   should take `self` as its sole argument and return the value. If you
#'   supply a `getter`, you are responsible for ensuring that it returns
#'   an object of the correct `class`; it will not be validated automatically.
#'
#'   If a property has a getter but doesn't have a setter, it is read only.
#' @param setter An optional function used to set the value. The function
#'   should take `self` and `value` and return a modified object.
#' @param validator The checkmate check-style function that will be used to
#'   verify the function. Arguments can be passed to this function using
#'   `...`.
#'
#'   The validator will be called after the `class` has been verified, so
#'   your code can assume that `value` has known type.
#' @param default When an object is created and the property is not supplied,
#'   what should it default to? If `NULL`, defaults to the "empty" instance
#'   of `class`.
#' @param name Property name, primarily used for error messages. Generally
#'   don't need to set this here, as it's more convenient to supply as a
#'   the element name when defining a list of properties. If both `name`
#'   and a list-name are supplied, the list-name will be used.
#' @returns An S7 property, i.e. a list with class `S7_property`.
#' @param ... arguments to be passed on to the validator function.
#'
#' @return
#' @export
#'
#' @examples
#'
#' has_bool <- new_class(
#'   "has_bool",
#'   properties = list(
#'     bool = new_checkmate_property(
#'       class = class_logical,
#'       validator = check_logical,
#'       len = 1
#'     )
#'   ))
#'
#' hb <- has_bool(bool=T)
#' hb_fail <- has_bool(bool=22)
new_checkmate_property <- function(class = class_any,
                                   getter = NULL,
                                   setter = NULL,
                                   validator = NULL,
                                   default = NULL,
                                   name = NULL,
                                   ...
) {
  if(is.null(validator) ||
     getNamespaceName(environment(validator)) != "checkmate") {
    msg <- paste0("Must provide a checkmate 'check' ",
                  "function as validator.\\n",
                  "Did you mean to use new_property?")
    stop(msg, call. = FALSE)
  }


  validator_name <- as.character(substitute(validator))
  if(startsWith(validator_name, "checkmate::")){
    validator_name <- strsplit(validator_name, "::", fixed=T)[[1]][2]
  }
  if(!startsWith(validator_name, "check")){
    msg <- paste0("Error in validator ", validator_name,
                  ": validator must be a 'check' function ",
                  "from the checkmate package.")
    stop(msg, call. = FALSE)
  }

  varargs <- list(...)
  reqargs <- formalArgs(validator)
  #TODO More checking for illegal args?
  if (!test_subset(names(varargs), reqargs)) {
    msg <- sprintf(
      paste0("`%s` in new_checkmate_property contains arguments %s",
             " not found in check function %s"),
      "...",
      paste0(setdiff(names(varargs), reqargs), collapse=', '),
      validator_name
    )
    stop(msg, call. = FALSE)
  }

  prop <- new_property(
    class = class,
    getter = getter,
    setter = setter,
    default = default,
    name = name,
    validator = validate_checks(validator, varargs)
  )
}

validate_checks <- function(check_fn, args){
  function(value) {
    args$x <- value
    res <- do.call(check_fn, args)
    if(isTRUE(res)){
      return(NULL)
    } else {
      return(res)
    }
  }
}

