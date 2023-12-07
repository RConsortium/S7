#' Define a new S7 class
#'
#' @description
#' A class specifies the properties (data) that each of its objects will
#' possess. The class, and its parent, determines which method will be used
#' when an object is passed to a generic.
#'
#' Learn more in `vignette("classes-objects")`
#'
#' @param name The name of the class, as a string. The result of calling
#'   `new_class()` should always be assigned to a variable with this name,
#'   i.e. `foo <- new_class("foo")`.
#' @param parent The parent class to inherit behavior from.
#'   There are three options:
#'
#'   * An S7 class, like [S7_object].
#'   * An S3 class wrapped by [new_S3_class()].
#'   * A base type, like [class_logical], [class_integer], etc.
#' @param package Package name. It is good practice to set the package
#'   name when exporting an S7 class from a package because it prevents
#'   clashes if two packages happen to export a class with the same
#'   name.
#'
#'   Setting `package` implies that the class is available for external use,
#'   so should be accompanied by exporting the constructor. Learn more
#'   in `vignette("packages")`.
#' @param abstract Is this an abstract class? An abstract class can not be
#'   instantiated.
#' @param constructor The constructor function. In most cases, you can rely
#'   on the default constructor, which will generate a function with one
#'   argument for each property.
#'
#'   A custom constructor should call `new_object()` to create the S7 object.
#'   The first argument, `.data`, should be an instance of the parent class
#'   (if used). The subsequent arguments are used to set the properties.
#' @param validator A function taking a single argument, `self`, the object
#'   to validate.
#'
#'   The job of a validator is to determine whether the object is valid,
#'   i.e. if the current property values form an allowed combination. The
#'   types of the properties are always automatically validated so the job of
#'   the validator is to verify that the _values_ of individual properties are
#'   ok (i.e. maybe a property should have length 1, or should always be
#'   positive), or that the _combination_ of values of multiple properties is ok.
#'   It is called after construction and whenever any property is set.
#'
#'   The validator should return `NULL` if the object is valid. If not, it
#'   should return a character vector where each element describes a single
#'   problem, using `@prop_name` to describe where the problem lies.
#'
#'   See `validate()` for more details, examples, and how to temporarily
#'   suppress validation when needed.
#' @param properties A named list specifying the properties (data) that
#'   belong to each instance of the class. Each element of the list can
#'   either be a type specification (processed by [as_class()]) or a
#'   full property specification created [new_property()].
#' @return A object constructor, a function that can be used to create objects
#'   of the given class.
#' @export
#' @examples
#' # Create an class that represents a range using a numeric start and end
#' range <- new_class("range",
#'   properties = list(
#'     start = class_numeric,
#'     end = class_numeric
#'   )
#' )
#' r <- range(start = 10, end = 20)
#' r
#' # get and set properties with @
#' r@start
#' r@end <- 40
#' r@end
#'
#' # S7 automatically ensures that properties are of the declared types:
#' try(range(start = "hello", end = 20))
#'
#' # But we might also want to use a validator to ensure that start and end
#' # are length 1, and that start is < end
#' range <- new_class("range",
#'   properties = list(
#'     start = class_numeric,
#'     end = class_numeric
#'   ),
#'   validator = function(self) {
#'     if (length(self@start) != 1) {
#'       "@start must be a single number"
#'     } else if (length(self@end) != 1) {
#'       "@end must be a single number"
#'     } else if (self@end < self@start) {
#'       "@end must be great than or equal to @start"
#'     }
#'   }
#' )
#' try(range(start = c(10, 15), end = 20))
#' try(range(start = 20, end = 10))
#'
#' r <- range(start = 10, end = 20)
#' try(r@start <- 25)
new_class <- function(
    name,
    parent = S7_object,
    package = NULL,
    properties = list(),
    abstract = FALSE,
    constructor = NULL,
    validator = NULL) {

  check_name(name)

  parent <- as_class(parent)

  # Don't check arguments for S7_object
  if (!is.null(parent)) {
    check_can_inherit(parent)
    if (!is.null(package)) {
      check_name(package)
    }
    if (!is.null(constructor)) {
      check_S7_constructor(constructor)
    }
    if (!is.null(validator)) {
      check_function(validator, alist(self = ))
    }
    if (abstract && !(parent@abstract || parent@name == "S7_object")) {
      stop("Abstract classes must have abstract parents")
    }
  }

  # Combine properties from parent, overriding as needed
  all_props <- attr(parent, "properties", exact = TRUE) %||% list()
  new_props <- as_properties(properties)
  all_props[names(new_props)] <- new_props

  if (is.null(constructor)) {
    constructor <- new_constructor(parent, all_props)
  }

  object <- constructor
  # Must synchronise with prop_names
  attr(object, "name") <- name
  attr(object, "parent") <- parent
  attr(object, "package") <- package
  attr(object, "properties") <- all_props
  attr(object, "abstract") <- abstract
  attr(object, "constructor") <- constructor
  attr(object, "validator") <- validator
  class(object) <- c("S7_class", "S7_object")

  global_variables(names(all_props))
  object
}
globalVariables(c("name", "parent", "package", "properties", "abstract", "constructor", "validator"))

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(nameOfClass, S7_class, S7_class_name)
S7_class_name <- function(x) {
  paste(c(x@package, x@name), collapse = "::")
}

check_S7_constructor <- function(constructor) {
  if (!is.function(constructor)) {
    stop("`constructor` must be a function", call. = FALSE)
  }

  method_call <- find_call(body(constructor), quote(new_object), packageName())
  if (is.null(method_call)) {
    stop("`constructor` must contain a call to `new_object()`", call. = FALSE)
  }
}

#' @export
print.S7_class <- function(x, ...) {
  props <- x@properties
  if (length(props) > 0) {
    prop_names <- format(names(props))
    prop_types <- format(vcapply(props, function(x) class_desc(x$class)))
    prop_fmt <- paste0(" $ ", prop_names, ": ", prop_types, "\n", collapse = "")
  } else {
    prop_fmt <- ""
  }

  cat(
    sprintf(
      paste0(
        "%s%s class\n",
        "@ parent     : %s\n",
        "@ constructor: %s\n",
        "@ validator  : %s\n",
        "@ properties :\n%s"
      ),
      class_desc(x),
      if (x@abstract) " abstract" else "",
      class_desc(x@parent),
      show_function(x@constructor, constructor = TRUE),
      if (!is.null(x@validator)) show_function(x@validator) else "<NULL>",
      prop_fmt
    ),
    sep = ""
  )
  invisible(x)
}

#' @export
str.S7_class <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  cat("<", paste0(class_dispatch(object), collapse = "/"), "> constructor", sep = "")
  cat("\n")

  if (nest.lev == 0) {
    str_nest(props(object), "@", ..., nest.lev = nest.lev)
  }
}

#' @export
c.S7_class <- function(...) {
  msg <- "Can not combine S7 class objects"
  stop(msg, call. = FALSE)
}

can_inherit <- function(x) is_base_class(x) || is_S3_class(x) || is_class(x)

check_can_inherit <- function(x, arg = deparse(substitute(x))) {
  if (!can_inherit(x)) {
    msg <- sprintf(
      "`%s` must be an S7 class, S3 class, or base type, not %s.",
      arg,
      class_friendly(x)
    )
    stop(msg, call. = FALSE)
  }

  if (is_base_class(x) && x$class == "environment") {
    stop("Can't inherit from an environment.", call. = FALSE)
  }
}

is_class <- function(x) inherits(x, "S7_class")

# Object ------------------------------------------------------------------

#' @param .parent,... Parent object and named properties used to construct the
#'   object.
#' @rdname new_class
#' @export
new_object <- function(.parent, ...) {
  class <- sys.function(-1)
  if (!inherits(class, "S7_class")) {
    stop("`new_object()` must be called from within a constructor")
  }
  if (class@abstract) {
    msg <- sprintf("Can't construct an object from abstract class <%s>", class@name)
    stop(msg)
  }

  args <- list(...)
  nms <- names(args)

  # TODO: Some type checking on `.parent`?
  object <- .parent
  attr(object, "S7_class") <- class
  class(object) <- class_dispatch(class)

  supplied_props <- nms[!vlapply(args, is_class_missing)]
  for (prop in supplied_props) {
    prop(object, prop, check = FALSE) <- args[[prop]]
  }

  # We have to fill in missing values after setting the initial properties,
  # because custom setters might set property values
  missing_props <- setdiff(nms, union(supplied_props, names(attributes(object))))
  for (prop in missing_props) {
    prop(object, prop, check = FALSE) <- prop_default(class@properties[[prop]])
  }

  # Don't need to validate if parent class already validated,
  # i.e. it's a non-abstract S7 class
  parent_validated <- inherits(class@parent, "S7_object") && !class@parent@abstract
  validate(object, recursive = !parent_validated)

  object
}

#' @export
print.S7_object <- function(x, ...) {
  str(x, ...)
  invisible(x)
}
#' @export
str.S7_object <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  cat(obj_desc(object))

  if (!is_S7_type(object)) {
    if (!typeof(object) %in% c("numeric", "integer", "character", "double"))
      cat(" ")

    attrs <- attributes(object)
    if (is.environment(object)) {
      attributes(object) <- NULL
    } else {
      attributes(object) <- list(names = names(object))
    }

    str(object, nest.lev = nest.lev)
    attributes(object) <- attrs
  } else {
    cat("\n")
  }

  str_nest(props(object), "@", ..., nest.lev = nest.lev)
}

#' Retrieve the S7 class of an object
#'
#' Given an S7 object, find it's class.
#'
#' @param object The S7 object
#' @returns An [S7 class][new_class].
#' @export
#' @examples
#' foo <- new_class("foo")
#' S7_class(foo())
S7_class <- function(object) {
  attr(object, "S7_class", exact = TRUE)
}
