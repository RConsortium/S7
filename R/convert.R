#' Convert an object from one type to another
#'
#' @description
#' `convert(from, to)` is a built-in generic for converting an object from
#' one type to another. It is special in four ways:
#'
#' * It uses double-dispatch, because conversion depends on both `from` and
#'   `to`.
#'
#' * It uses non-standard dispatch because `to` is a class, not an object.
#'
#' * It doesn't use inheritance for the `to` argument. To understand
#'   why, imagine you have written methods to objects of various types to
#'   `classParent`. If you then create a new `classChild` that inherits from
#'   `classParent`, you can't expect the methods written for `classParent`
#'   to work because those methods will return `classParent` objects, not
#'   `classChild` objects.
#'
#' * `from` uses ordinary inheritance, so a method registered on a parent class
#'   is also used for its children, with two exceptions. If `from` is already an
#'   instance of `to`, it's returned unchanged and no dispatch is needed.
#'   When upcasting (i.e. `to` is an ancestor of `from`), `convert()` will
#'   never dispatch to a method registered on `to` or one of its ancestors,
#'   because such a method would downcast.
#'
#' `convert()` provides three default implementations:
#'
#' 1. When `from` inherits from `to`, it strips any properties that `from`
#'    possesses that `to` does not (upcasting).
#' 2. When `to` inherits from `from`, it creates a new object of class `to`,
#'    copying over existing properties from `from` and initializing new
#'    properties of `to` (downcasting).
#' 3. When `to` is a base type (e.g. [class_integer] or [class_character]) and
#'    neither of the above apply, it calls the corresponding `as.*()` function
#'    (e.g. `as.integer()` or `as.character()`). This mirrors the convention
#'    that `as.*()` coercion sits below `convert()`, so you can rely on it as a
#'    fallback but still override it with a more specific method.
#'
#' If you are converting an object solely for the purposes of accessing a method
#' on a superclass, you probably want [super()] instead. See its docs for more
#' details.
#'
#' ## S3 & S4
#'
#' `convert()` plays a similar role to the convention of defining `as.foo()`
#' functions/generics in S3, and to `as()`/`setAs()` in S4.
#'
#' @param from An S7 object to convert.
#' @param to An S7 class specification, passed to [as_class()].
#' @param ... Other arguments passed to custom `convert()` methods. For
#'   downcasting, these can be used to override existing properties or set new
#'   ones. As a convenience, you can supply a single unnamed list instead of
#'   individual name-value pairs, which makes it easy to override properties
#'   programmatically.
#' @return Either `from` coerced to class `to`, or an error if the coercion
#'   is not possible.
#' @export
#' @examples
#' Foo1 := new_class(properties = list(x = class_integer))
#' Foo2 := new_class(Foo1, properties = list(y = class_double))
#'
#' # Upcasting: S7 provides a default implementation for coercing an object
#' # to one of its parent classes:
#' convert(Foo2(x = 1L, y = 2), to = Foo1)
#'
#' # Downcasting: S7 also provides a default implementation for coercing an
#' # object to one of its child classes:
#' convert(Foo1(x = 1L), to = Foo2)
#' convert(Foo1(x = 1L), to = Foo2, y = 2.5)  # Set new property
#' convert(Foo1(x = 1L), to = Foo2, x = 2L, y = 2.5)  # Override existing and set new
#'
#' # Converting to a base type falls back to the corresponding `as.*()`:
#' convert(1.5, to = class_character)
#' convert(c("1", "2"), to = class_integer)
#'
#' # For all other cases, you'll need to provide your own.
#' try(convert(Foo1(x = 1L), to = class_integer))
#'
#' method(convert, list(Foo1, class_integer)) <- function(from, to) {
#'   from@x
#' }
#' convert(Foo1(x = 1L), to = class_integer)
#'
#' # Conversion does not respect inheritance for `to`, so if we define a
#' # convert method for integer to Foo1
#' method(convert, list(class_integer, Foo1)) <- function(from, to) {
#'   Foo1(x = from)
#' }
#' convert(1L, to = Foo1)
#'
#' # Converting to Foo2 will still error
#' try(convert(1L, to = Foo2))
#' # This is probably not surprising because Foo2 also needs some value
#' # for `@y`, but it definitely makes dispatch for convert() special
#'
#' # Conversely, `convert()` *does* use inheritance for `from`, so a method
#' # registered on a parent class is also used for its children. This holds
#' # even when upcasting, where it overrides the default property stripping:
#' Bar1 := new_class(properties = list(label = class_character))
#' Bar2 := new_class(Bar1)
#' Bar3 := new_class(Bar2)
#' method(convert, list(Bar2, Bar1)) <- function(from, to, ...) {
#'   Bar1(label = "from a Bar2 or one of its children")
#' }
#' convert(Bar2(), to = Bar1)
#' convert(Bar3(), to = Bar1) # Bar3 inherits Bar2, so the Bar2 method is used
#'
#' # This `from`-inheritance is limited to classes more specific than `to`. A
#' # method whose `from` is a *parent* of `to` would downcast, so it is skipped.
#' # For example, this method downcasts a Foo1 to a Foo2:
#' Foo3 := new_class(Foo2, properties = list(z = class_double))
#' method(convert, list(Foo1, Foo2)) <- function(from, to, ...) Foo2(y = -1)
#'
#' # Upcasting a Foo3 to a Foo2 ignores that inherited downcasting method,
#' # keeping `x` and `y` and dropping `z`, rather than resetting `y` to -1:
#' convert(Foo3(x = 1L, y = 2, z = 3), to = Foo2)
convert <- function(from, to, ...) {
  to <- as_class(to)
  check_can_inherit(to)

  method <- convert_method(from, to)
  if (!is.null(method)) {
    method(from, to, ...)
  } else if (class_inherits(from, to)) {
    convert_up(from, to)
  } else if (is_down_cast(from, to)) {
    dots <- collect_dots(...)
    convert_down(from, to, dots)
  } else if (is_base_class(to)) {
    base_coerce(from, to, ...)
  } else if (is_S4_coerce(from, to)) {
    convert_S4(from, to, ...)
  } else {
    msg <- paste_c(
      "Can't find method with dispatch classes:\n",
      c("- from: ", obj_desc(from), "\n"),
      c("- to  : ", class_desc(to))
    )
    stop2(msg)
  }
}

# Resolve the `convert()` method for converting `from` to `to`, or `NULL` if
# there's no registered method (so `convert()` falls back to its defaults).
# See convert docs for the motivation for this design.
convert_method <- function(from, to) {
  from_dispatch <- obj_dispatch(from)
  to_class <- class_register(to)

  for (i in seq_along(from_dispatch)) {
    if (from_dispatch[[i]] == to_class) {
      if (i == 1L) {
        return(function(from, to, ...) from)
      }
      from_dispatch <- from_dispatch[seq_len(i - 1L)]
      break
    }
  }

  .Call(method_, convert, list(from_dispatch, to_class), environment(), FALSE)
}

convert_up <- function(from, to, call = sys.call(-1L)) {
  check_not_environment(from, "convert()", call = call)

  from_class <- S7_class(from)
  if (is_class(from_class)) {
    from_props <- prop_storage_names(from)
  } else {
    # `from` is a base, S3, or S4 object, so it has no S7 properties
    from_props <- character()
  }

  if (is_base_class(to)) {
    from <- zap_attr(from, c(from_props, "_S7_class", "S7_class", "class"))
  } else if (is_S3_class(to)) {
    if (class_is_abstract(to)) {
      msg <- sprintf("Can't convert to abstract class <%s>.", to$class[[1]])
      stop2(msg, call = call)
    }
    from <- zap_attr(from, c(from_props, "_S7_class", "S7_class"))
    class(from) <- to$class
  } else if (is_class(to)) {
    to_prop_nms <- names(to@properties)
    to_props <- prop_storage_rename(to_prop_nms)
    if (to@abstract) {
      msg <- sprintf("Can't convert to abstract class <%s>.", to@name)
      stop2(msg, call = call)
    }

    is_s4_subclass <- isS4(from) &&
      is_class(from_class) &&
      !identical(class(from)[[1L]], S7_class_name(from_class))
    s4_slot_attrs <- if (is_s4_subclass) {
      setdiff(methods::slotNames(from), c(to_props, ".Data"))
    } else {
      character()
    }
    from <- zap_attr(
      from,
      c(setdiff(from_props, to_props), s4_slot_attrs, "_S7_class", "S7_class")
    )
    attr(from, "_S7_class") <- to
    if (is_s4_subclass) {
      from <- suppressWarnings(`class<-`(from, class_dispatch(to)))
    } else {
      class(from) <- class_dispatch(to)
    }
  } else if (
    is_S4_class(to) && is_class(from_class) && class_extends(from_class, to)
  ) {
    from <- S4_as_validity_class(from, to)
  } else if (is_S4_coerce(from, to)) {
    from <- convert_S4(from, to)
  } else {
    stop2("Unreachable.")
  }
  from
}

is_down_cast <- function(x, class) {
  class_dispatch_extends(obj_dispatch(x), class_dispatch(class))
}

convert_down <- function(from, to, user_args = list()) {
  from_class <- S7_class(from)

  if (!is_class(from_class)) {
    if (isS4(from)) {
      from_slot_values <- S4_initialize_values(from)
      from_slot_names <- names(from_slot_values)

      to_constructor_arg_names <- names(formals(to))
      if (!"..." %in% to_constructor_arg_names) {
        from_slot_names <- intersect(from_slot_names, to_constructor_arg_names)
      }

      from_slot_names <- setdiff(from_slot_names, names(user_args))
      constructor_args <- c(from_slot_values[from_slot_names], user_args)
      return(do.call(to, constructor_args))
    }

    # `from` is a base or S3 object; pass it as `.data` to the constructor
    user_args$.data <- from
    return(do.call(to, user_args))
  }

  # Use `from` as a prototype/seed when constructing `to`: copy over property
  # values from `from` and supply them as arguments to the `to` constructor.

  from_props <- from_class@properties
  from_props <- Filter(Negate(prop_is_read_only), from_props)
  from_prop_names <- names(from_props)

  # If `to` constructor has no `...`, only use properties that match its args
  to_constructor_arg_names <- names(formals(to))
  if (!"..." %in% to_constructor_arg_names) {
    from_prop_names <- intersect(from_prop_names, to_constructor_arg_names)
  }

  # Drop properties overridden by user-supplied arguments
  from_prop_names <- setdiff(from_prop_names, names(user_args))

  from_prop_values <- props(from, from_prop_names)
  constructor_args <- c(from_prop_values, user_args)

  do.call(to, constructor_args)
}

s4_to_name <- function(x) {
  if (is_S4_class(x)) {
    return(x@className)
  }

  class <- S4_registered_class_or_null(x, environment(x))
  if (is.null(class)) {
    NULL
  } else {
    class@className
  }
}

is_S4_coerce <- function(from, to) {
  # can loosen this restriction once convert() has default base targets
  if (!inherits_S4(from) && !is_S4_class(to)) {
    return(FALSE)
  }

  to_name <- s4_to_name(to)
  !is.null(to_name) && methods::canCoerce(from, to_name)
}

convert_S4 <- function(from, to, ...) {
  methods::as(from, s4_to_name(to), ...)
}

# Converted to S7_generic onLoad in order to avoid dependency between files
on_load_make_convert_generic <- function() {
  convert <<- S7_generic(
    convert,
    name = "convert",
    dispatch_args = c("from", "to")
  )
}
