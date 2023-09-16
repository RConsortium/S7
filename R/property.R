#' Define a new property
#'
#' @description
#' A property defines a named component of an object. Properties are
#' typically used to store (meta) data about an object, and are often
#' limited to a data of a specific `class`.
#'
#' By specifying a `getter` and/or `setter`, you can make the property
#' "dynamic" so that it's computed when accessed or has some non-standard
#' behaviour when modified.
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
#' @param validator A function taking a single argument, `value`, the value
#'   to validate.
#'
#'   The job of a validator is to determine whether the property value is valid.
#'   It should return `NULL` if the object is valid, or if it's not valid,
#'   a single string describing the problem. The message should not include the
#'   name of the property as this will be automatically appended to the
#'   beginning of the message.
#'
#'   The validator will be called after the `class` has been verified, so
#'   your code can assume that `value` has known type.
#' @param default When an object is created and the property is not supplied,
#'   what should it default to? If `NULL`, defaults to the "empty" instance
#'   of `class`.
#' @param name Property name, primarily used for error messages. Used
#'   primrarily for testing as it is set automatically when using a list of
#'   properties.
#' @returns An S7 property, i.e. a list with class `S7_property`.
#' @export
#' @examples
#' # Simple properties store data inside an object
#' pizza <- new_class("pizza", properties = list(
#'   slices = new_property(class_numeric, default = 10)
#' ))
#' my_pizza <- pizza(slices = 6)
#' my_pizza@slices
#' my_pizza@slices <- 5
#' my_pizza@slices
#'
#' your_pizza <- pizza()
#' your_pizza@slices
#'
#' # Dynamic properties can compute on demand
#' clock <- new_class("clock", properties = list(
#'   now = new_property(getter = function(self) Sys.time())
#' ))
#' my_clock <- clock()
#' my_clock@now; Sys.sleep(1)
#' my_clock@now
#' # This property is read only
#' try(my_clock@now <- 10)
#'
#' # These can be useful if you want to deprecate a property
#' person <- new_class("person", properties = list(
#'   first_name = class_character,
#'   firstName = new_property(
#'      getter = function(self) {
#'        warning("@firstName is deprecated; please use @first_name instead", call. = FALSE)
#'        self@first_name
#'      },
#'      setter = function(self, value) {
#'        warning("@firstName is deprecated; please use @first_name instead", call. = FALSE)
#'        self@first_name <- value
#'        self
#'      }
#'    )
#' ))
#' hadley <- person(first_name = "Hadley")
#' hadley@firstName
#' hadley@firstName <- "John"
#' hadley@first_name
new_property <- function(class = class_any,
                         getter = NULL,
                         setter = NULL,
                         validator = NULL,
                         default = NULL,
                         name = NULL) {
  class <- as_class(class)
  if (!is.null(default) && !class_inherits(default, class)) {
    msg <- sprintf("`default` must be an instance of %s, not a %s", class_desc(class), obj_desc(default))
    stop(msg)
  }

  if (!is.null(getter)) {
    check_function(getter, alist(self = ))
  }
  if (!is.null(setter)) {
    check_function(setter, alist(self = , value = ))
  }
  if (!is.null(validator)) {
    check_function(validator, alist(value = ))
  }

  out <- list(
    name = name,
    class = class,
    getter = getter,
    setter = setter,
    validator = validator,
    default = default
  )
  class(out) <- "S7_property"

  out
}

is_property <- function(x) inherits(x, "S7_property")

#' @export
print.S7_property <- function(x, ...) {
  cat("<S7_property> \n")
  str_nest(x, "$", ...)
}

#' @export
str.S7_property <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object, ..., nest.lev = nest.lev)
}

prop_default <- function(prop) {
  prop$default %||% class_construct(prop$class)
}

#' Get/set a property
#'
#' - `prop(x, "name")` / `prop@name` get the value of the a property,
#'   erroring if it the property doesn't exist.
#' - `prop(x, "name") <- value` / `prop@name <- value` set the value of
#'   a property.
#'
#' @param object An object from a S7 class
#' @param name The name of the parameter as a character. Partial matching
#'   is not performed.
#' @param value A new value for the property. The object is automatically
#'   checked for validity after the replacement is done.
#' @return `prop()` and `@` return the value of the property.
#'   `prop<-()` and `@<-` are called for their side-effects and return
#'    the modified object, invisibly.
#' @export
#' @examples
#' horse <- new_class("horse", properties = list(
#'   name = class_character,
#'   colour = class_character,
#'   height = class_numeric
#' ))
#' lexington <- horse(colour = "bay", height = 15, name = "Lex")
#' lexington@colour
#' prop(lexington, "colour")
#'
#' lexington@height <- 14
#' prop(lexington, "height") <- 15
prop <- function(object, name) {
  check_is_S7(object)

  if (!prop_exists(object, name)) {
    stop(prop_error_unknown(object, name), call. = FALSE)
  } else {
    prop_val(object, name)
  }
}

# Internal helper that assumes the property exists
prop_val <- function(object, name) {
  val <- attr(object, name, exact = TRUE)
  if (is.null(val)) {
    prop <- prop_obj(object, name)
    if (!is.null(prop$getter)) {
      val <- prop$getter(object)
    }
  }
  val
}

# Get underlying property object from class
prop_obj <- function(object, name) {
  class <- S7_class(object)
  attr(class, "properties")[[name]]
}

#' @rdname prop
#' @param check If `TRUE`, check that `value` is of the correct type and run
#'   [validate()] on the object before returning.
#' @export
`prop<-` <- local({
  # This flag is used to avoid infinite loops if you are assigning a property from a setter function
  setter_property <- NULL

  function(object, name, check = TRUE, value) {
    check_is_S7(object)

    prop <- prop_obj(object, name)
    if (is.null(prop)) {
      stop(prop_error_unknown(object, name), call. = FALSE)
    }

    if (!is.null(prop$getter) && is.null(prop$setter)) {
      msg <- sprintf("Can't set read-only property %s@%s", obj_desc(object), name)
      stop(msg, call. = FALSE)
    }

    if (!is.null(prop$setter) && !identical(setter_property, name)) {
      setter_property <<- name
      on.exit(setter_property <<- NULL, add = TRUE)
      object <- prop$setter(object, value)
    } else {
      if (isTRUE(check)) {
        error <- prop_validate(prop, value, object)
        if (!is.null(error)) {
          stop(error, call. = FALSE)
        }
      }

      attr(object, name) <- value
    }

    if (isTRUE(check) && is.null(setter_property)) {
      validate(object, properties = FALSE)
    }

    invisible(object)
  }
})

prop_error_unknown <- function(object, prop_name) {
  sprintf("Can't find property %s@%s", obj_desc(object), prop_name)
}

prop_validate <- function(prop, value, object = NULL) {
  if (!class_inherits(value, prop$class)) {
    sprintf("%s must be %s, not %s",
      prop_label(object, prop$name),
      class_desc(prop$class),
      obj_desc(value)
    )
  } else if (!is.null(prop$validator)) {
    val <- prop$validator(value)
    if (!is.null(val)) {
      paste0(prop_label(object, prop$name), " ", val)
    } else {
      NULL
    }
  } else {
    NULL
  }
}

prop_label <- function(object, name) {
  sprintf("%s@%s", if (!is.null(object)) obj_desc(object) else "", name)
}

#' @rdname prop
#' @usage object@name
#' @aliases @
#' @rawNamespace if (getRversion() >= "4.3.0") S3method(base::`@`, S7_object) else export("@")
`@.S7_object` <- prop

# Note: we need to explicitly refer to base with "base::`@`" in the
# namespace directive to ensure the method is registered in the correct place.
# Otherwise, loadNamespace()/registerS3method() gets confused by the
# presence of a closure w/ the name of the generic (`@`) in the R7 namespace,
# and incorrectly assumes that R7::`@` is the generic and registers the
# method in the package namespace instead of base::.__S3MethodsTable__.

`@` <- function(object, name) {
  if (inherits(object, "S7_object")) {
    name <- as.character(substitute(name))
    prop(object, name)
  } else {
    name <- substitute(name)
    do.call(base::`@`, list(object, name))
  }
}

#' @rawNamespace S3method("@<-",S7_object)
`@<-.S7_object` <- function(object, name, value) {
  nme <- as.character(substitute(name))
  prop(object, nme) <- value

  invisible(object)
}


#' Property introspection
#'
#' - `prop_names(x)` returns the names of the properties
#' - `prop_exists(x, "prop")` returns `TRUE` iif `x` has property `prop`.
#'
#' @inheritParams prop
#' @returns `prop_names()` returns a character vector; `prop_exists()` returns
#'   a single `TRUE` or `FALSE`.
#' @export
#' @examples
#' foo <- new_class("foo", properties = list(a = class_character, b = class_integer))
#' f <- foo()
#'
#' prop_names(f)
#' prop_exists(f, "a")
#' prop_exists(f, "c")
prop_names <- function(object) {
  check_is_S7(object)

  if (inherits(object, "S7_class")) {
    # S7_class isn't a S7_class (somewhat obviously) so we fake the property names
    c("name", "parent", "package", "properties", "abstract", "constructor", "validator")
  } else {
    class <- S7_class(object)
    props <- attr(class, "properties", exact = TRUE)
    if (length(props) == 0) {
      character()
    } else {
      names(props)
    }
  }
}

# .AtNames not exported on r-devel yet, causes installation failure
#' @rawNamespace if (getRversion() >= "4.3.0" && !is.null(asNamespace("utils")$.AtNames)) S3method(utils::.AtNames,S7_object)
.AtNames.S7_object <- function(x, pattern = "") {
  # utils::findMatches gives `R CMD check` warning on current r-devel
  asNamespace("utils")$findMatches(pattern, prop_names(x))
}

#' @rdname prop_names
#' @export
prop_exists <- function(object, name) {
  check_is_S7(object)
  name %in% prop_names(object)
}

#' Get/set multiple properties
#'
#' - `props(x)` returns all properties.
#' - `props(x) <- list(name1 = val1, name2 = val2)` modifies an existing object
#'   by setting multiple properties simultaneously.
#' - `set_props(x, name1 = val1, name2 = val2)` creates a copy of an existing
#'   object with new values for the specified properties.
#'
#' @importFrom stats setNames
#' @inheritParams prop
#' @returns A named list of property values.
#' @export
#' @examples
#' horse <- new_class("horse", properties = list(
#'   name = class_character,
#'   colour = class_character,
#'   height = class_numeric
#' ))
#' lexington <- horse(colour = "bay", height = 15, name = "Lex")
#'
#' props(lexington)
#' props(lexington) <- list(height = 14, name = "Lexington")
#' lexington
props <- function(object) {
  check_is_S7(object)
  prop_names <- prop_names(object)
  if (length(prop_names) == 0) {
    list()
  } else {
    setNames(lapply(prop_names, prop, object = object), prop_names)
  }
}
#' @rdname props
#' @export
#' @param value A named list of values. The object is checked for validity
#'   only after all replacements are performed.
`props<-` <- function(object, value) {
  check_is_S7(object)
  stopifnot(is.list(value))

  for (name in names(value)) {
    prop(object, name, check = FALSE) <- value[[name]]
  }
  validate(object)

  object
}

#' @export
#' @param ... Name-value pairs given property to modify and new value.
#' @rdname props
set_props <- function(object, ...) {
  props(object) <- list(...)
  object
}

as_properties <- function(x) {
  if (length(x) == 0) {
    return(list())
  }

  if (!is.list(x)) {
    stop("`properties` must be a list", call. = FALSE)
  }

  out <- Map(as_property, x, names2(x), seq_along(x))
  names(out) <- names2(x)

  if (anyDuplicated(names(out))) {
    stop("`properties` names must be unique", call. = FALSE)
  }

  out
}

as_property <- function(x, name, i) {
  if (name == "") {
    msg <- sprintf("`property[[%i]]` is missing a name", i)
    stop(msg, call. = FALSE)
  }

  if (is_property(x)) {
    x$name <- name
    x
  } else {
    class <- as_class(x, arg = paste0("property$", name))
    new_property(x, name = name)
  }
}
