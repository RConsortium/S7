#' @importFrom utils modifyList
R7_class <- function(name, parent = R7_object, constructor = NULL, validator = function(x) NULL, properties = list()) {

  parent <- as_class(parent)
  if (is_union(parent) || is_s4_class(parent)) {
    stop("`parent` must be an R7 class, S3 class, or base type")
  }

  # Combine properties from parent, overriding as needed
  properties <- modifyList(
    attr(parent, "properties", exact = TRUE) %||% list(),
    as_properties(properties)
  )

  if (is.null(constructor)) {
    constructor <- new_constructor(parent, properties)
  }

  object <- constructor
  attr(object, "name") <- name
  attr(object, "parent") <- parent
  attr(object, "properties") <- properties
  attr(object, "constructor") <- constructor
  attr(object, "validator") <- validator
  class(object) <- c("R7_class", "R7_object")

  global_variables(names(properties))
  object
}

is_class <- function(x) inherits(x, "R7_class")

#' Create a new R7 class
#'
#' A class specifies the properties (data) that each of its objects will
#' possess. The class, and its parent, determines which method will be used
#' when an object is passed to a generic.
#'
#' @param name The name of the class, as a string.
#' @param parent The parent class.
#'
#'   * To inherit behaviour from an R7 class, pass the class object.
#'   * To inherit behaviour from a base type, pass the function you'd use
#'     to construct the object, e.g. `character`, `integer`.
#'
#' @param constructor The constructor function. This is optional, unless
#'   you want to control which properties can be set on constructor.
#' @param validator A function used to determine whether or not an object
#'   is valid. This is called automatically after construction, and
#'   whenever any property is set. It should return `NULL` if the object is
#'   valid, and otherwise return a character vector of problems.
#' @param properties A list specifying the properties (data) that
#'   every object of the class will possess. Each property can either be
#'   a named string (specifying the class), or a call to [new_property()],
#'   allowing greater flexibility.
#' @return A object constructor, a function that can be used to create objects
#'   of the given class.
#' @export
#' @examples
#' # Create an class that represents a range using a numeric start and end
#' range <- new_class("range",
#'   properties = list(
#'     start = "numeric",
#'     end = "numeric"
#'   )
#' )
#' r <- range(start = 10, end = 20)
#' r
#' # get and set properties with @
#' r@start
#' r@end <- 40
#' r@end
#'
#' # Use a validator to ensure that start and end are both length 1,
#' # and that start is < end
#' range <- new_class("range",
#'   properties = list(
#'     start = "numeric",
#'     end = "numeric"
#'   ),
#'   validator = function(x) {
#'     if (length(x@start) != 1) {
#'       "@start must be a single number"
#'     } else if (length(x@end) != 1) {
#'       "@end must be a single number"
#'     } else if (x@end < x@start) {
#'       "@end must be great than or equal to @start"
#'     }
#'   }
#' )
#' try(range(start = c(10, 15), end = 20))
#' try(range(start = 20, end = 10))
#' # Type validation is performed automatically in R7
#' try(range(start = "hello", end = 20))
new_class <- function(name, parent = R7_object, constructor = NULL, validator = function(x) NULL, properties = list()) {
  R7_class(name = name, parent = parent, constructor = constructor, validator = validator, properties = properties)
}

#' Retrieve all of the class names for a class
#'
#' @param object The R7 object to query
#' @return A character vector of all the class names for a given R7 class.
#' @export
class_names <- function(object) {
  parent <- object
  classes <- character()
  while(!is.null(parent)) {
    if (inherits(parent, "R7_union")) {
      for (class in parent@classes) {
        classes <- c(classes, class_names(class))
      }
    } else if (inherits(parent, "R7_class")) {
      classes <- c(classes, parent@name, "R7_object")
    } else {
      classes <- c(classes, parent)
    }
    parent <- prop_safely(parent, "parent")
  }
  unique(classes, fromLast = TRUE)
}

#' Standard class specifications
#'
#' Can be:
#' * An R7 class object or class union.
#' * An S3 class object, created by `s3_class()`.
#' * An S4 class object.
#' * A base type specified either with its constructor (`logical`, `integer`,
#'   `double` etc) or its name (`"logical"`, `"integer"`, "`double`" etc).
#'
#' @param x The name of the R7 class
#' @param envir The environment to look for the name
#' @param unions Include unions?
#' @export
as_class <- function(x, envir = parent.frame()) {
  if (is.null(x)) {
    x
  } else if (is_class(x)) {
    x
  } else if (is_union(x)) {
    x
  } else if (is_s3_class(x)) {
    x
  } else if (is_s4_class(x)) {
    x
  } else if (is.function(x)) {
    candidate <- Filter(function(y) identical(x, y), base_constructors)
    if (length(candidate) != 1) {
      stop("Could not find base class corresponding to supplied constructor function")
    }
    base_classes[[names(candidate)]]
  } else if (is.character(x) && length(x) == 1) {
    if (x %in% names(base_classes)) {
      base_classes[[x]]
    } else {
      stop(sprintf("Can't find base class called '%s'", x))
    }
  } else {
    print(x)
    stop("Invalid class specification")
  }
}

class_type <- function(x) {
  if (is_class(x)) {
    if (hasName(base_classes, x@name)) {
      "r7_base"
    } else {
      "r7"
    }
  } else if (is_union(x)) {
    "r7_union"
  } else if (is.null(x)) {
    "NULL"
  } else if (is_s3_class(x)) {
    "s3"
  } else if (isS4(x)) {
    "s4"
  } else {
    stop("`x` is not standard R7 class")
  }
}

obj_type <- function(x) {
  if (inherits(x, "R7_object")) {
    "r7"
  } else if (isS4(x)) {
    "s4"
  } else if (is.object(x)) {
    "s3"
  } else {
    "base"
  }
}
obj_desc <- function(x) {
  switch(obj_type(x),
   base = fmt_classes(typeof(x)),
   s3 = fmt_classes(class(x)),
   s4 = fmt_classes(class(x)),
   r7 = fmt_classes(object_class(x)@name)
  )
}

class_desc <- function(x) {
  switch(class_type(x),
    NULL = "",
    s3 = fmt_classes(class(x)[[1]]),
    s4 = fmt_classes(class(x)),
    r7 = fmt_classes(x@name),
    r7_base = fmt_classes(x@name),
    r7_union = paste(unlist(lapply(x@classes, class_desc)), collapse = " u "),
  )
}

class_inherits <- function(x, what) {
  switch(class_type(what),
    NULL = TRUE,
    s3 = is_s3(x) && inherits(x, class(what)),
    s4 = isS4(x) && methods::is(x, what),
    r7 = inherits(x, "R7_object") && inherits(x, what@name),
    r7_base = inherits(x, what@name),
    r7_union = any(vlapply(what@classes, class_inherits, x = x))
  )
}

#' @export
print.R7_class <- function(x, ...) {
  props <- x@properties
  if (length(props) > 0) {
    prop_names <- format(names(props))
    prop_types <- format(vcapply(props, function(x) class_desc(x$class)), justify = "right")
    prop_fmt <- paste0(paste0(" $", prop_names, " ", prop_types, collapse = "\n"), "\n")
  } else {
    prop_fmt <- ""
  }
  parent <- prop_safely(x, "parent")
  parent <- prop_safely(parent, "name") %||% parent %||% ""

  cat(sprintf("<R7_class>\n@name %s\n@parent <%s>\n@properties\n%s", x@name, parent, prop_fmt), sep = "")
}

#' Declare an S3 class vector
#'
#' @export
s3_class <- function(class) {
  if (!is.character(class)) {
    stop("Class must be a character vector")
  }
  structure(class, class = "r7_s3_class")
}

#' @export
#' @rdname s3_class
is_s3_class <- function(x) {
  inherits(x, "r7_s3_class")
}

is_s4_class <- function(x) {
  isS4(x) && methods::isClass(x)
}
