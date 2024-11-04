#' Convert an object from one type to another
#'
#' @description
#' `convert(from, to)` is a built-in generic for converting an object from
#' one type to another. It is special in three ways:
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
#' `convert()` provides two default implementations:
#'
#' 1. When `from` inherits from `to`, it strips any properties that `from`
#'    possesses that `to` does not (downcasting).
#' 2. When `to` is a subclass of `from`'s class, it creates a new object of
#'    class `to`, copying over existing properties from `from` and initializing
#'    new properties of `to` (upcasting).
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
#' @param ... Other arguments passed to custom `convert()` methods. For upcasting,
#'   these can be used to override existing properties or set new ones.
#' @return Either `from` coerced to class `to`, or an error if the coercion
#'   is not possible.
#' @export
#' @examples
#' Foo1 <- new_class("Foo1", properties = list(x = class_integer))
#' Foo2 <- new_class("Foo2", Foo1, properties = list(y = class_double))
#'
#' # Downcasting: S7 provides a default implementation for coercing an object
#' # to one of its parent classes:
#' convert(Foo2(x = 1L, y = 2), to = Foo1)
#'
#' # Upcasting: S7 also provides a default implementation for coercing an object
#' # to one of its child classes:
#' convert(Foo1(x = 1L), to = Foo2)
#' convert(Foo1(x = 1L), to = Foo2, y = 2.5)  # Set new property
#' convert(Foo1(x = 1L), to = Foo2, x = 2L, y = 2.5)  # Override existing and set new
#'
#' # For all other cases, you'll need to provide your own.
#' try(convert(Foo1(x = 1L), to = class_integer))
#'
#' method(convert, list(Foo1, class_integer)) <- function(from, to) {
#'   from@x
#' }
#' convert(Foo1(x = 1L), to = class_integer)
#'
#' # Note that conversion does not respect inheritance so if we define a
#' # convert method for integer to foo1
#' method(convert, list(class_integer, Foo1)) <- function(from, to) {
#'   Foo1(x = from)
#' }
#' convert(1L, to = Foo1)
#'
#' # Converting to Foo2 will still error
#' try(convert(1L, to = Foo2))
#' # This is probably not surprising because foo2 also needs some value
#' # for `@y`, but it definitely makes dispatch for convert() special
convert <- function(from, to, ...) {
  to <- as_class(to)
  check_can_inherit(to)

  dispatch <- list(obj_dispatch(from), class_register(to))
  convert <- .Call(method_, convert, dispatch, environment(), FALSE)

  if (!is.null(convert)) {
    convert(from, to, ...)
  } else if (class_inherits(from, to)) {
    from_class <- S7_class(from)
    if (is.null(from_class)) {
      from_props <- character()
    } else {
      from_props <- names(from_class@properties)
    }

    if (is_base_class(to)) {
      from <- zap_attr(from, c(from_props, "S7_class", "class"))
    } else if (is_S3_class(to)) {
      from <- zap_attr(from, c(from_props, "S7_class"))
      class(from) <- to$class
    } else if (is_class(to)) {
      from <- zap_attr(from, setdiff(from_props, names(to@properties)))
      attr(from, "S7_class") <- to
      class(from) <- class_dispatch(to)
    } else {
      stop("Unreachable")
    }
    from
  } else if (is_parent_instance(from, to)) {
    # We're up-casting, using `from` as a prototype/seed when constructing `to`.
    # Essentially, we copy over property values from `from` and supply them as
    # arguments to the `to` constructor.

    # Get properties of 'from' class
    from_props <- S7_class(from)@properties

    # Remove read-only properties
    from_props <- Filter(Negate(prop_is_read_only), from_props)
    from_prop_names <- names(from_props)

    # Check if 'to' constructor can accept all properties
    to_constructor_arg_names <- names(formals(to))
    if (!"..." %in% to_constructor_arg_names) {
      # If no ..., only use properties that match constructor arguments
      from_prop_names <- intersect(from_prop_names, to_constructor_arg_names)
    }

    # Remove properties that are overridden in user-supplied arguments
    user_args <- list(...)
    from_prop_names <- setdiff(from_prop_names, names(user_args))

    # Extract property values from 'from'
    from_prop_values <- props(from, from_prop_names)

    # Combine property values with user-supplied arguments
    constructor_args <- c(from_prop_values, user_args)

    # Create and return new object of class 'to'
    do.call(to, constructor_args)
  } else {
    msg <- paste0(
      "Can't find method for generic `convert()` with dispatch classes:\n",
      "- from: ", obj_desc(from), "\n",
      "- to  : ", class_desc(to), "\n"
    )
    stop(msg, call. = FALSE)
  }
}

# Converted to S7_generic onLoad in order to avoid dependency between files
on_load_make_convert_generic <- function() {
  convert <<- S7_generic(
    convert,
    name = "convert",
    dispatch_args = c("from", "to")
  )
}

is_parent_instance <- function(x, class) {
  inherits(x, setdiff(class_dispatch(class), "S7_object"))
}
