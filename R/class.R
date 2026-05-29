#' Define a new S7 class
#'
#' @description
#' A class specifies the properties (data) that each of its objects will
#' possess. The class, and its parent, determines which method will be used
#' when an object is passed to a generic.
#'
#' Learn more in `vignette("classes-objects")`
#'
#' @param name The name of the class, as a string. (We recommend using
#'   CamelCase for S7 class names, but it is not required.)
#'
#'   The result of calling `new_class()` should always be assigned to a variable
#'   with this name, i.e. `Foo <- new_class("Foo")`. This object both represents
#'   the class and is used to construct new instances of the class.
#' @param parent The parent class to inherit behavior from.
#'   There are three options:
#'
#'   * An S7 class, like [S7_object].
#'   * An S3 class wrapped by [new_S3_class()].
#'   * A base type, like [class_logical], [class_integer], etc.
#' @param package Package name. This is automatically resolved if the class is
#'   defined in a package, and `NULL` otherwise.
#'
#'   Note, if the class is intended for external use, the constructor should be
#'   exported. Learn more in `vignette("packages")`.
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
#' Range <- new_class("Range",
#'   properties = list(
#'     start = class_numeric,
#'     end = class_numeric
#'   )
#' )
#' r <- Range(start = 10, end = 20)
#' r
#' # get and set properties with @
#' r@start
#' r@end <- 40
#' r@end
#'
#' # S7 automatically ensures that properties are of the declared types:
#' try(Range(start = "hello", end = 20))
#'
#' # But we might also want to use a validator to ensure that start and end
#' # are length 1, and that start is < end
#' Range <- new_class("Range",
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
#' try(Range(start = c(10, 15), end = 20))
#' try(Range(start = 20, end = 10))
#'
#' r <- Range(start = 10, end = 20)
#' try(r@start <- 25)
new_class <- function(
  name,
  parent = S7_object,
  package = topNamespaceName(parent.frame()),
  properties = list(),
  abstract = FALSE,
  constructor = NULL,
  validator = NULL
) {
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
    if (
      abstract &&
        (!is_class(parent) || !(parent@abstract || parent@name == "S7_object"))
    ) {
      stop("Abstract classes must have abstract parents.")
    }
  }

  # Combine properties from parent, overriding as needed
  all_props <- attr(parent, "properties", exact = TRUE) %||% list()
  new_props <- as_properties(properties)
  check_prop_names(new_props)
  all_props[names(new_props)] <- new_props

  if (is.null(constructor)) {
    constructor <- new_constructor(
      parent,
      all_props,
      envir = parent.frame(),
      package = package
    )
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
globalVariables(c(
  "name",
  "parent",
  "package",
  "properties",
  "abstract",
  "constructor",
  "validator"
))

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(nameOfClass, S7_class, S7_class_name)
S7_class_name <- function(x) {
  paste(c(x@package, x@name), collapse = "::")
}

check_S7_constructor <- function(constructor) {
  if (!is.function(constructor)) {
    stop("`constructor` must be a function.", call. = FALSE)
  }

  method_call <- find_call(body(constructor), quote(new_object), packageName())
  if (is.null(method_call)) {
    stop("`constructor` must contain a call to `new_object()`.", call. = FALSE)
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
  cat(
    "<",
    paste0(class_dispatch(object), collapse = "/"),
    "> constructor",
    sep = ""
  )
  cat("\n")

  if (nest.lev == 0) {
    str_nest(props(object), "@", ..., nest.lev = nest.lev)
  }
}

#' @export
c.S7_class <- function(...) {
  msg <- "Can not combine S7 class objects."
  stop(msg)
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
}

is_class <- function(x) inherits(x, "S7_class")

check_parent <- function(parent, class) {
  parent_class <- class@parent
  if (is.null(parent_class)) {
    stop(
      "`.parent` must not be supplied when class has no parent.",
      call. = FALSE
    )
  }

  # Ignore abstract classes since you can't supply an instance
  if (is_class(parent_class) && parent_class@abstract) {
    return()
  }

  if (class_inherits(parent, parent_class)) {
    return()
  }
  msg <- sprintf(
    "`.parent` must be an instance of %s, not %s.",
    class_desc(parent_class),
    obj_desc(parent)
  )
  stop(msg, call. = FALSE)
}

# Object ------------------------------------------------------------------

#' @param .parent,... Parent object and named properties used to construct the
#'   object.
#' @rdname new_class
#' @export
new_object <- function(.parent, ...) {
  class <- sys.function(-1)
  if (!inherits(class, "S7_class")) {
    stop("`new_object()` must be called from within a constructor.")
  }
  if (class@abstract) {
    msg <- sprintf(
      "Can't construct an object from abstract class <%s>.",
      class@name
    )
    stop(msg)
  }

  if (!missing(.parent)) {
    check_parent(.parent, class)
  }

  args <- list(...)
  if ("" %in% names2(args)) {
    stop("All arguments to `...` must be named.")
  }

  has_setter <- vlapply(class@properties[names(args)], prop_has_setter)

  # We must awkwardly operate on `.parent` rather than binding to a local
  # variable; since otherwise the extra binding causes ALTREP-wrapped values to
  # be materialised when byte-compiled (#607).
  attrs <- c(
    list(class = class_dispatch(class), S7_class = class),
    args[!has_setter],
    attributes(.parent)
  )
  attrs <- attrs[!duplicated(names(attrs))]
  attributes(.parent) <- attrs

  # invoke custom property setters
  prop_setter_vals <- args[has_setter]
  for (name in names(prop_setter_vals)) {
    prop(.parent, name, check = FALSE) <- prop_setter_vals[[name]]
  }

  # Don't need to validate if parent class already validated,
  # i.e. it's a non-abstract S7 class
  parent_validated <- inherits(class@parent, "S7_object") &&
    !class@parent@abstract
  validate_from(.parent, parent = if (parent_validated) class@parent)

  .parent
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

  if (is.environment(object)) {
    # Can't use S7_data() with environments
    cat(" ", format.default(object), "\n", sep = "")
  } else if (!is_S7_type(object)) {
    if (!typeof(object) %in% c("numeric", "integer", "character", "double")) {
      cat(" ")
    }

    str(S7_data(object), nest.lev = nest.lev)
  } else {
    cat("\n")
  }

  str_nest(props(object), "@", ..., nest.lev = nest.lev)
}

#' Retrieve the class specification of an object
#'
#' @description
#' `S7_class()` returns a [class specification][as_class] for any R object, in a form
#' that can be passed to [method()] or used in any S7 dispatch context.
#'
#' * For S7 objects, the [S7 class][new_class].
#' * For S3 objects, a [new_S3_class()] wrapping `class(x)`.
#' * For S4 objects, the S4 class.
#' * For base types, the matching `class_*` (e.g. [class_integer]).
#' * For missing arguments, returns [class_missing].
#'
#' @param object Any R object.
#' @returns A class specification.
#' @export
#' @examples
#' Foo <- new_class("Foo")
#' S7_class(Foo())
#'
#' # Also works on non-S7 objects
#' S7_class(1L)
#' S7_class("x")
#' S7_class(mean)
#' S7_class(factor("a"))
S7_class <- function(object) {
  switch(
    obj_type(object),
    missing = class_missing,
    S7 = attr(object, "S7_class", exact = TRUE),
    S4 = methods::getClass(class(object)),
    S3 = new_S3_class(class(object)),
    base = base_S7_class(object)
  )
}


check_prop_names <- function(properties, error_call = sys.call(-1L)) {
  # these attributes have special C handlers in base R
  forbidden <- c(
    "names",
    "dim",
    "dimnames",
    "class",
    "tsp",
    "comment",
    "row.names",
    "..."
  )
  forbidden <- intersect(forbidden, names(properties))
  if (length(forbidden)) {
    msg <- paste0(
      "Property can't be named: ",
      paste0(forbidden, collapse = ", "),
      "."
    )
    stop(simpleError(msg, error_call))
  }
}
