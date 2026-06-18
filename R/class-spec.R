#' Standard class specifications
#'
#' This is used as the interface between S7 and R's other OO systems, allowing
#' you to use S7 classes and methods with base types, informal S3 classes, and
#' formal S4 classes.
#'
#' @param x A class specification. One of the following:
#'   * An S7 class (created by [new_class()]).
#'   * An S7 union (created by [new_union()]).
#'   * An S3 class (created by [new_S3_class()]).
#'   * An S4 class (created by [methods::getClass()] or [methods::new()]).
#'   * A base class, like [class_logical], [class_integer], or [class_double].
#'   * `NULL`.
#'   * A "special", either [class_missing] or [class_any].
#' @param arg Argument name used when generating errors.
#' @keywords internal
#' @export
#' @return A standardised class: either `NULL`, an S7 class, an S7 union,
#'   as [new_S3_class], or a S4 class.
#' @examples
#' as_class(class_logical)
#' as_class(new_S3_class("factor"))
as_class <- function(x, arg = deparse(substitute(x))) {
  error_base <- sprintf("Can't convert `%s` to a valid class.", arg)

  if (is_foundation_class(x)) {
    x
  } else if (is.null(x)) {
    # NULL is handled specially because you can't assign a class to it,
    # so it can't be wrapped in new_base_class
    x
  } else if (isS4(x)) {
    S4_to_S7_class(x, error_base, call = sys.call(-1L))
  } else {
    msg <- c(
      error_base,
      sprintf(
        "Class specification must be one of the following, not a %s:",
        obj_desc(x)
      ),
      " * An S7 class object",
      " * An S3 class object (from `new_S3_class()`)",
      " * An S4 class object",
      " * A base class"
    )

    stop2(msg)
  }
}

is_foundation_class <- function(x) {
  is_class(x) ||
    is_union(x) ||
    is_base_class(x) ||
    is_S3_class(x) ||
    is_external_class(x) ||
    is_class_missing(x) ||
    is_class_any(x)
}

class_type <- function(x) {
  if (is.null(x)) {
    "NULL"
  } else if (is_class_missing(x)) {
    "missing"
  } else if (is_class_any(x)) {
    "any"
  } else if (is_base_class(x)) {
    "S7_base"
  } else if (is_class(x)) {
    "S7"
  } else if (is_union(x)) {
    "S7_union"
  } else if (is_S3_class(x)) {
    "S7_S3"
  } else if (is_external_class(x)) {
    "S7_external"
  } else if (is_S4_class(x)) {
    "S4"
  } else {
    stop2("`x` is not a standard S7 class.", call = NULL)
  }
}

class_friendly <- function(x) {
  switch(
    class_type(x),
    NULL = "NULL",
    missing = "a missing argument",
    any = "any type",
    S4 = "an S4 class",
    S7 = "an S7 class",
    S7_base = "a base type",
    S7_union = "an S7 union",
    S7_S3 = "an S3 class",
    S7_external = "an external S7 class",
  )
}

class_construct <- function(.x, ...) {
  class_constructor(.x)(...)
}


class_construct_expr <- function(.x, envir = NULL, package = NULL) {
  f <- class_constructor(.x)

  # For S7 class constructors with a non-NULL @package property
  # Instead of inlining the full class definition, use either
  # `pkgname::classname()` or `classname()`
  if (is_class(f) && !is.null(f@package)) {
    # Check if the class can be resolved as a bare symbol without pkgname::
    # Note: During package build, using pkg::class for a package's own symbols
    # will raise an error from `::`.
    if (identical(package, f@package)) {
      return(call(f@name))
    } else {
      # namespace the pkgname::classname() call
      cl <- as.call(list(quote(`::`), as.name(f@package), as.name(f@name)))

      # check the call evaluates to f.
      # This will error if package is not installed or object is not exported.
      f2 <- eval(cl, baseenv())
      if (!identical(f, f2)) {
        msg <- sprintf(
          "`%s::%s` is not identical to the class with the same @package and @name properties.",
          f@package,
          f@name
        )
        stop2(msg, call = NULL)
      }
      return(as.call(list(cl)))
    }
  }

  # If the constructor is a closure wrapping a simple expression, try
  # to extract the expression
  # (mostly for nicer printing and introspection.)

  # can't unwrap if the closure is potentially important
  fe <- environment(f)
  if (!identical(fe, baseenv()) && !identical(fe, asNamespace("S7"))) {
    return(as.call(list(f)))
  }

  # special case for `class_missing`
  if (identical(body(f) -> fb, quote(expr = ))) {
    return(quote(expr = ))
  }

  # `new_object()` must be called from the class constructor, can't
  # be safely unwrapped
  if ("new_object" %in% all.names(fb)) {
    return(as.call(list(f)))
  }

  # maybe unwrap body if it is a single expression wrapped in `{`
  if (length(fb) == 2L && identical(fb[[1L]], quote(`{`))) {
    fb <- fb[[2L]]
  }

  # If all the all the work happens in the promise to the `.data` arg,
  # return the `.data` expression.
  ff <- formals(f)
  if (
    (identical(fb, quote(.data))) &&
      identical(names(ff), ".data")
  ) {
    return(ff$.data)
  }

  # if all the work happens in the function body, return the body.
  if (is.null(ff)) {
    return(fb)
  }

  #else, return a call to the constructor
  as.call(list(f))
}

class_constructor <- function(.x) {
  switch(
    class_type(.x),
    any = ,
    NULL = new_function(env = baseenv()),
    missing = new_function(, quote(expr = ), baseenv()),
    S4 = function(...) methods::new(.x, ...),
    S7 = .x,
    S7_base = .x$constructor,
    S7_union = class_constructor(.x$classes[[1]]),
    S7_S3 = .x$constructor,
    S7_external = class_constructor(resolve_external_class_req(.x)),
    stop2(sprintf("Can't construct %s.", class_friendly(.x)), call = NULL)
  )
}

class_validate <- function(class, object) {
  validator <- switch(
    class_type(class),
    S4 = function(object) {
      check <- methods::validObject(object, test = TRUE)
      if (isTRUE(check)) NULL else check
    },
    S7 = class@validator,
    S7_base = class$validator,
    S7_S3 = class$validator,
    S7_external = return(class_validate(
      resolve_external_class_req(class),
      object
    )),
    NULL
  )

  if (is.null(validator)) {
    NULL
  } else {
    validator(object)
  }
}

#' Format a class specification as a string
#'
#' `S7_class_desc()` turns any [class specification][as_class] into a short,
#' human-readable, string, suitable for use in user-facing messages.
#'
#' @param class A [class specification][as_class].
#' @returns A string.
#' @export
#' @examples
#' S7_class_desc(class_integer)
#' S7_class_desc(new_S3_class("data.frame"))
#' S7_class_desc(class_integer | class_double)
#' S7_class_desc(NULL)
S7_class_desc <- function(class) {
  class <- as_class(class)
  class_desc(class)
}

class_desc <- function(x) {
  switch(
    class_type(x),
    NULL = "<NULL>",
    missing = "<MISSING>",
    any = "<ANY>",
    S4 = paste0("S4<", x@className, ">"),
    S7 = paste0("<", S7_class_name(x), ">"),
    S7_base = paste0("<", x$class, ">"),
    S7_union = oxford_or(unlist(lapply(x$classes, class_desc))),
    S7_S3 = paste0("S3<", paste0(x$class, collapse = "/"), ">"),
    S7_external = paste0("<", x$class_name, ">"),
  )
}

# Vector of class names; used in method introspection
class_dispatch <- function(x) {
  if (is_class(x) && x@name == "S7_object") {
    return("S7_object")
  }

  switch(
    class_type(x),
    NULL = "NULL",
    missing = "MISSING",
    any = character(),
    S4 = S4_class_dispatch(methods::extends(x)),
    S7 = c(S7_class_name(x), class_dispatch(x@parent)),
    S7_base = c(x$class, "S7_object"),
    S7_S3 = c(x$class, "S7_object"),
    S7_external = class_dispatch(resolve_external_class_req(x)),
    stop2("Unsupported class type.", call = NULL)
  )
}

# Class name when registering an S7 method
class_register <- function(x) {
  switch(
    class_type(x),
    NULL = "NULL",
    missing = "MISSING",
    any = "ANY",
    S4 = S4_class_name(x),
    S7 = S7_class_name(x),
    S7_base = x$class,
    S7_S3 = x$class[[1]],
    S7_external = x$class_name,
    stop2("Unsupported class type.", call = NULL)
  )
}

# Used when printing method signature to generate executable code
class_deparse <- function(x) {
  switch(
    class_type(x),
    "NULL" = "NULL",
    missing = "class_missing",
    any = "class_any",
    S4 = as.character(x@className),
    S7 = S7_class_name(x),
    S7_base = paste0("class_", x$class),
    S7_union = {
      classes <- vcapply(x$classes, class_deparse)
      paste0("new_union(", paste(classes, collapse = ", "), ")")
    },
    S7_S3 = paste0("new_S3_class(", deparse1(x$class), ")"),
    S7_external = {
      args <- c(deparse1(x$package), deparse1(x$name))
      if (!is.null(x$version)) {
        args <- c(args, paste0("version = ", deparse1(x$version)))
      }
      sprintf("new_external_class(%s)", paste(args, collapse = ", "))
    },
  )
}

class_inherits <- function(x, what) {
  switch(
    class_type(what),
    "NULL" = is.null(x),
    missing = FALSE,
    any = TRUE,
    S4 = isS4(x) && methods::is(x, what),
    S7 = inherits(x, "S7_object") && inherits(x, S7_class_name(what)),
    S7_base = what$class == base_class(x),
    S7_union = any(vlapply(what$classes, class_inherits, x = x)),
    S7_S3 = !isS4(x) && class_dispatch_extends(what$class, class(x)),
    S7_external = inherits(x, "S7_object") &&
      (inherits(x, what$class_name) ||
        class_inherits(x, resolve_external_class_req(what))),
  )
}

# Is every instance of `child` guaranteed to also be an instance of `parent`?
# Used to check that a child class only narrows the type of a property
class_extends <- function(child, parent) {
  if (is_class_any(parent) || union_contains_any(parent)) {
    # as a parent, `class_any` accepts every child class
    TRUE
  } else if (is_class_any(child)) {
    # as a child, `class_any` only allows `class_any` as a parent
    FALSE
  } else if (is_union(child)) {
    # A union child extends `parent` only if every one of its members does.
    all(vlapply(child$classes, class_extends, parent = parent))
  } else if (is_union(parent)) {
    # A non-union child extends a union parent if it extends any of its members.
    any(vlapply(parent$classes, class_extends, child = child))
  } else if (is.null(child) && !is.null(parent)) {
    # as a child, NULL can only extend NULL
    FALSE
  } else if (is.null(parent)) {
    # as a parent, NULL only accepts NULL
    is.null(child)
  } else if (is_external_class(child)) {
    child <- resolve_external_class_req(child)
    class_extends(child, parent)
  } else if (is_external_class(parent)) {
    parent <- resolve_external_class_req(parent)
    class_extends(child, parent)
  } else if (is_S4_class(child) || is_S4_class(parent)) {
    is_S4_class(child) &&
      is_S4_class(parent) &&
      methods::extends(child@className, parent@className)
  } else if (is_class(parent) && parent@name == "S7_object") {
    is_class(child)
  } else {
    # handle S7, S3, and base types.
    class_dispatch_extends(class_dispatch(parent), class_dispatch(child))
  }
}

obj_type <- function(x) {
  if (identical(x, quote(expr = ))) {
    "missing"
  } else if (inherits(x, "S7_object")) {
    "S7"
  } else if (isS4(x)) {
    "S4"
  } else if (is.object(x)) {
    "S3"
  } else {
    "base"
  }
}
obj_desc <- function(x) {
  switch(
    obj_type(x),
    missing = "MISSING",
    base = paste0("<", typeof(x), ">"),
    S3 = paste0("S3<", paste(class(x), collapse = "/"), ">"),
    S4 = paste0("S4<", class(x), ">"),
    S7 = paste0("<", class(x)[[1]], ">")
  )
}
obj_dispatch <- function(x) {
  switch(
    obj_type(x),
    missing = "MISSING",
    base = base_class(x),
    S3 = class(x),
    S4 = S4_class_dispatch(methods::getClass(class(x))),
    S7 = class(x) # = class_dispatch(S7_class(x))
  )
}

# helpers -----------------------------------------------------------------

# Does `child`'s dispatch extend `parent`'s? Subclassing only ever prepends
# more specific classes, so `parent`'s classes must form the tail of `child`'s.
# S7 wrappers of base/S3 types append "S7_object", which we ignore.
class_dispatch_extends <- function(parent, child) {
  parent <- drop_S7_object(parent)
  child <- drop_S7_object(child)
  n <- length(parent)
  length(child) >= n && identical(child[length(child) - n + seq_len(n)], parent)
}

drop_S7_object <- function(x) {
  n <- length(x)
  if (n > 0 && x[[n]] == "S7_object") x[-n] else x
}

union_contains_any <- function(x) {
  is_union(x) && any(vlapply(x$classes, is_class_any))
}

# Suppress @className false positive
globalVariables("className")
