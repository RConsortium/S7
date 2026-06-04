#' Register an S7, S3, or union class with S4
#'
#' If you want to use [method<-] to register a method for an S4 generic with
#' an S7 class that does not extend an S4 class, or an S3 class created by
#' [new_S3_class()], you need to call `S4_register()` once. Classes created by
#' [new_class()] with an S4 parent are registered automatically.
#' Use `S4_register_contains()` when you want an S4 class to extend an S7 class
#' with `contains=`. This registers the S7 class as an old class with known
#' attributes so that S7 properties are represented as S4 slots.
#'
#' @param class An S7 class created with [new_class()], or, for
#'   `S4_register()` only, an S3 class created with [new_S3_class()] or an S7
#'   union created with [new_union()].
#' @param env Expert use only. Environment where S4 class will be registered.
#' @returns
#' Both functions are called for their side effects and invisibly return the
#' registered S4 class name.
#' @export
#' @examples
#' methods::setGeneric("S4_generic", function(x) {
#'   standardGeneric("S4_generic")
#' })
#'
#' Foo <- new_class("Foo")
#' S4_register(Foo)
#' method(S4_generic, Foo) <- function(x) "Hello"
#'
#' S4_generic(Foo())
#'
#' S4Foo <- new_class("S4Foo", properties = list(x = class_numeric), package = "S7")
#' S4Foo_S4 <- S4_register_contains(S4Foo)
#' methods::setClass("S4Child", contains = S4Foo_S4)
S4_register <- function(class, env = parent.frame()) {
  if (is_class(class)) {
    classes <- class_dispatch(class)
  } else if (is_S3_class(class)) {
    classes <- class$class
  } else if (is_union(class)) {
    return(invisible(S4_register_union(class, topenv(env))))
  } else {
    msg <- sprintf(
      "`class` must be an S7 class, S3 class, or S7 union, not a %s.",
      obj_desc(class)
    )
    stop2(msg)
  }

  methods::setOldClass(classes, where = topenv(env))
  invisible(classes[1L])
}

S4_register_union <- function(class, env) {
  name <- S4_union_name(class, env)
  methods::setClassUnion(
    name,
    vcapply(class$classes, S4_class, S4_env = env),
    where = env
  )
  name
}

S4_class <- function(x, S4_env, call = sys.call(-1L)) {
  switch(
    class_type(x),
    `NULL` = "NULL",
    missing = "missing",
    any = "ANY",
    S7_base = base_to_S4(x$class),
    S4 = as.character(x@className),
    S7 = as.character(
      S4_registered_class(x, S4_env, call = call)@className
    ),
    S7_S3 = as.character(
      S4_registered_class(x, S4_env, call = call)@className
    ),
    S7_union = S4_union_class(x, S4_env)
  )
}

# S4 dispatch uses `class()` to find a method, but `class(1.5)` is "numeric",
# not "double", so registering under "double" silently misses real doubles.
# Mapping to "numeric" catches doubles but also matches integers too. There's
# no clean S4 way to say "doubles only" and this seems likely to be what
# people want.
base_to_S4 <- function(class) {
  switch(class, double = "numeric", class)
}

S4_registered_class <- function(x, S4_env, call = sys.call(-1L)) {
  class <- tryCatch(
    methods::getClass(class_register(x), where = S4_env),
    error = function(err) NULL
  )
  if (is.null(class)) {
    msg <- sprintf(
      "Class has not been registered with S4; please call S4_register(%s).",
      class_deparse(x)
    )
    stop2(msg, call = call)
  }
  class
}

S4_union_class <- function(x, S4_env) {
  if (identical(x, class_numeric)) {
    return("numeric")
  }

  name <- S4_union_name(x, S4_env)
  if (methods::isClass(name, where = S4_env)) {
    return(name)
  }

  name <- S4_find_union(x, S4_env)
  if (!is.null(name)) {
    return(name)
  }

  msg <- sprintf(
    "Class union has not been registered with S4; please call S4_register(%s).",
    class_deparse(x)
  )
  stop(msg, call. = FALSE)
}

S4_union_name <- function(x, S4_env) {
  paste0(vcapply(x$classes, S4_class, S4_env = S4_env), collapse = "_OR_")
}

S4_find_union <- function(x, S4_env) {
  members <- vcapply(x$classes, S4_class, S4_env = S4_env)
  supers <- lapply(members, S4_direct_superclasses, S4_env = S4_env)
  candidates <- base::Reduce(base::intersect, supers)
  matches <- base::Filter(
    function(candidate) S4_union_matches(candidate, members, S4_env),
    candidates
  )
  if (length(matches) == 0) {
    return(NULL)
  }

  matches[1L]
}

S4_direct_superclasses <- function(class, S4_env) {
  class <- methods::getClass(class, where = S4_env)
  contains <- class@contains
  contains <- base::Filter(function(x) x@distance == 1, contains)
  names(contains)
}

S4_union_matches <- function(class, members, S4_env) {
  class <- methods::getClass(class, where = S4_env)
  if (!methods::isClassUnion(class)) {
    return(FALSE)
  }

  setequal(S4_union_members(class), members)
}

S4_union_members <- function(class) {
  subclasses <- base::Filter(function(x) x@distance == 1, class@subclasses)
  vcapply(subclasses, function(x) as.character(x@subClass))
}

S4_ancestor <- function(class) {
  parent_class <- attr(class, "parent", exact = TRUE)
  while (is_class(parent_class)) {
    parent_class <- attr(parent_class, "parent", exact = TRUE)
  }
  if (is_S4_class(parent_class)) parent_class
}

S7_extends_S4 <- function(class) {
  !is.null(S4_ancestor(class))
}

inherits_S4 <- function(x) {
  isS4(x) ||
    {
      klass <- S7_class(x)
      !is.null(klass) && S7_extends_S4(klass)
    }
}

S4_register_subclass <- function(class, env) {
  where <- topenv(env)
  subclasses <- S4_subclasses(class)
  old_classes <- c(subclasses, "S7_object")
  if (length(subclasses) > 1L) {
    methods::setOldClass(old_classes, where = where)
  } else {
    methods::setOldClass(
      old_classes,
      S4Class = S4_register_prototype_class(class, where),
      where = where
    )
    methods::setValidity(subclasses[1L], S4_validate_old_class, where = where)
    methods::setMethod(
      "initialize",
      subclasses[1L],
      S4_initialize,
      where = where
    )
  }

  parent_class <- S4_ancestor(class)
  methods::setAs(
    from = subclasses[1L],
    to = parent_class@className,
    def = S7_subclass_as_parent(parent_class),
    where = where
  )

  invisible()
}

S7_subclass_as_parent <- function(parent_class) {
  function(from) {
    if (methods::isVirtualClass(parent_class)) {
      return(from)
    }
    class(from) <- parent_class@className
    asS4(from)
  }
}

#' @rdname S4_register
#' @export
S4_register_contains <- function(class, env = parent.frame()) {
  if (!is_class(class)) {
    msg <- sprintf("`class` must be an S7 class, not a %s.", obj_desc(class))
    stop(msg, call. = FALSE)
  }

  where <- topenv(env)
  classes <- class_dispatch(class)
  if (!methods::isClass(classes[1L], where = where)) {
    S4_register(class, where)
  }
  class_name <- S4_register_with_props(class, where)
  invisible(class_name)
}

S4_register_with_props <- function(class, env) {
  where <- topenv(env)
  class_name <- S4_register_contains_name(class)
  contains <- S4_class(class, where)
  properties <- class@properties
  properties <- properties[setdiff(
    names(properties),
    S4_slot_names(contains, where)
  )]

  methods::setClass(
    Class = class_name,
    slots = lapply(properties, S4_property_class, S4_env = where),
    contains = c(contains, "S7_object::S4Slots", "VIRTUAL"),
    prototype = S4_properties_prototype(properties, class, where, TRUE),
    where = where
  )
  methods::setValidity(class_name, S4_validate_shim, where = where)
  methods::setAs(
    from = class_name,
    to = contains,
    def = S4_slot_class_as_parent,
    where = where
  )

  class_name
}

S4_slot_class_as_parent <- function(from) {
  from <- asS3(from, complete = FALSE)
  class(from) <- class_dispatch(S7_class(from))
  from
}

S4_slot_names <- function(class, S4_env) {
  names(methods::getClass(class, where = S4_env)@slots)
}

S4_register_contains_name <- function(class) {
  paste0(S7_class_name(class), "::S4Slots")
}

S4_property_class <- function(prop, S4_env) {
  if (prop_is_dynamic(prop) || prop_has_setter(prop)) {
    msg <- sprintf(
      "Can't register property %s as an S4 slot because it has a custom %s.",
      prop$name,
      if (prop_is_dynamic(prop)) "getter" else "setter"
    )
    stop(msg, call. = FALSE)
  }
  S4_class(prop$class, S4_env)
}

S4_properties_prototype <- function(
  properties,
  class,
  env,
  include_S7_class = FALSE
) {
  args <- list()
  for (name in names(properties)) {
    value <- S4_property_prototype(properties[[name]], env, class@package)
    if (length(value) != 0L) {
      args[[name]] <- value[[1L]]
    }
  }
  if (include_S7_class) {
    args$S7_class <- class
  }
  do.call(methods::prototype, args)
}

S4_property_prototype <- function(prop, env, package) {
  tryCatch(
    {
      value <- prop_default(prop, env, package)
      if (is.call(value) || is.symbol(value)) {
        value <- eval(value, env)
      }
      list(value)
    },
    error = function(cnd) {
      if (!is.null(prop$default)) {
        stop(cnd)
      }
      list()
    }
  )
}

S4_subclasses <- function(class) {
  subclasses <- character()
  while (is_class(class)) {
    subclasses <- c(subclasses, S7_class_name(class))
    class <- class@parent
    if (is_S4_class(class)) {
      return(subclasses)
    }
  }
  character()
}

S4_validate_old_class <- function(object) {
  if (isS4(object)) {
    # covered by S4_validate_shim()
    return(TRUE)
  }

  S4_validate(object)
}

S4_validate_shim <- function(object) {
  S4_validate(object)
}

S4_validate <- function(object) {
  if (!S7_inherits(object)) {
    return(sprintf(
      "object with S4 class %s is not an S7 object",
      dQuote(class(object)[1L])
    ))
  }

  tryCatch(
    {
      validate(object)
      TRUE
    },
    error = function(cnd) conditionMessage(cnd)
  )
}

S4_initialize <- function(.Object, ...) {
  args <- list(...)
  if (length(args) == 0) {
    return(.Object)
  }

  nms <- names2(args)
  prop_nms <- prop_names(.Object)
  vals <- list()
  data_part <- NULL
  for (arg in args[nms == ""]) {
    arg_vals <- S4_initialize_values(arg)
    if (".Data" %in% names(arg_vals)) {
      data_part <- arg
    }
    arg_vals <- arg_vals[names(arg_vals) %in% prop_nms]
    vals <- modify_list(vals, arg_vals)
  }
  named_args <- args[nms != ""]
  vals <- modify_list(vals, named_args)
  if (".Data" %in% names(named_args)) {
    data_part <- vals$.Data
  }

  if (!is.null(data_part)) {
    .Object <- S4_initialize_data_part(data_part, .Object)
  }

  props(.Object) <- vals
  .Object
}

S4_initialize_values <- function(object) {
  if (S7_inherits(object)) {
    props(object)
  } else if (isS4(object)) {
    slots <- methods::slotNames(object)
    stats::setNames(lapply(slots, methods::slot, object = object), slots)
  } else {
    attrs <- attributes(object) %||% list()
    attrs$class <- NULL
    if (is.object(object)) {
      attrs$.S3Class <- class(object)
    }
    c(list(.Data = unclass(object)), attrs)
  }
}

S4_initialize_data_part <- function(value, object) {
  incoming <- attributes(value) %||% list()
  incoming$class <- NULL
  attributes(value) <- modify_list(attributes(object), incoming)
  value
}

S4_register_prototype_class <- function(class, env = parent.frame()) {
  where <- topenv(env)
  classes <- class_dispatch(class)

  parent_class <- class@parent
  stopifnot(is_S4_class(parent_class))

  methods::setClass(
    Class = classes[1L],
    contains = c(parent_class@className, "VIRTUAL"),
    where = where
  )

  classes[1L]
}

is_S4_class <- function(x) inherits(x, "classRepresentation")

S4_to_S7_class <- function(x, error_base = "", call = sys.call(-1L)) {
  # Silence R CMD check false positives
  distance <- subClass <- className <- package <- NULL

  # Convert generator function to class
  if (methods::is(x, "classGeneratorFunction")) {
    return(S4_to_S7_class(
      methods::getClass(x@className),
      error_base,
      call = call
    ))
  }

  if (methods::isClassUnion(x)) {
    subclasses <- Filter(function(y) y@distance == 1, x@subclasses)
    subclasses <- lapply(subclasses, function(x) methods::getClass(x@subClass))
    do.call("new_union", subclasses)
  } else if (methods::is(x, "classRepresentation")) {
    if (x@package == "methods") {
      basic_classes <- S4_basic_classes()
      if (hasName(basic_classes, x@className)) {
        return(basic_classes[[x@className]])
      }
    }
    if (is_oldClass(x)) {
      new_S3_class(methods::extends(x))
    } else {
      x
    }
  } else {
    msg <- sprintf(
      "Unsupported S4 object: must be a class generator or a class definition, not a %s.",
      obj_desc(x)
    )
    stop2(paste0(error_base, msg), call = call)
  }
}

S4_slot_properties <- function(class) {
  properties <- Map(S4_slot_property, class@slots, names(class@slots))
  names(properties) <- names(class@slots)
  properties
}

S4_slot_property <- function(class, name) {
  new_property(
    class = S4_to_S7_class(methods::getClass(class)),
    name = name
  )
}

S4_basic_classes <- function() {
  list(
    NULL = NULL,
    logical = class_logical,
    integer = class_integer,
    double = class_double,
    numeric = class_numeric,
    character = class_character,
    complex = class_complex,
    raw = class_raw,
    list = class_list,
    expression = class_expression,
    vector = class_vector,
    `function` = class_function,
    environment = class_environment,
    name = class_name,
    call = class_call,
    data.frame = class_data.frame,
    Date = class_Date,
    factor = class_factor,
    POSIXct = class_POSIXct,
    POSIXlt = class_POSIXlt,
    POSIXt = class_POSIXt,
    # matrix = class_matrix,
    # array = class_array,
    formula = class_formula
  )
}

S4_class_dispatch <- function(x) {
  x <- methods::getClass(x)
  self <- S4_class_name(x)

  # Find class objects for super classes
  extends <- unname(methods::extends(x, fullInfo = TRUE))
  extends <- Filter(function(x) methods::is(x, "SClassExtension"), extends)
  classes <- lapply(extends, function(x) methods::getClass(x@superClass))

  # Remove unions: S7 handles them in method registration, not dispatch.
  classes <- Filter(
    function(x) !methods::is(x, "ClassUnionRepresentation"),
    classes
  )
  # Remove specially named union base classes
  classes <- Filter(
    function(x) !x@className %in% c("oldClass", "vector"),
    classes
  )

  c(self, vcapply(classes, S4_class_name))
}

is_oldClass <- function(x) {
  methods::extends(x, "oldClass") &&
    x@className %in% attr(x@prototype, ".S3Class")
}

S4_class_name <- function(x) {
  if (is_oldClass(x)) {
    return(x@className)
  }

  class <- x@className
  package <- x@package %||% attr(class, "package")

  if (identical(package, "methods") && class %in% names(S4_basic_classes())) {
    class
  } else if (is.null(package) || identical(package, ".GlobalEnv")) {
    paste0("S4/", class)
  } else {
    paste0("S4/", package, "::", class)
  }
}

S4_package_name <- function(f, env) {
  if (methods::getPackageName(topenv(env), create = FALSE) == f@package) {
    ## current ns might not be loaded yet, catch here
    return(f@package)
  }

  name <- as.character(f@generic)
  generic_in_its_package <- methods::isGeneric(
    name,
    where = asNamespace(f@package)
  )
  if (generic_in_its_package) {
    return(f@package)
  }

  # generic was defined for a function from a different package, like base
  find_package_with_symbol(name, env, exclude = f@package) %||%
    stop2(
      sprintf(
        "Failed to find originating package for S4 generic '%s' in imports.",
        f@generic
      ),
      call = NULL
    )
}

find_package_with_symbol <- function(name, env, exclude = NULL) {
  imports <- getNamespaceImports(topenv(env))
  pkgs <- setdiff(names(imports), exclude)
  for (pkg in pkgs) {
    if (
      (isTRUE(imports[[pkg]]) && name %in% getNamespaceExports(pkg)) ||
        name %in% imports[[pkg]]
    ) {
      return(pkg)
    }
  }
}

S4_remove_classes <- function(classes, where = parent.frame()) {
  where <- topenv(where)
  for (class in classes) {
    if (methods::isClass(class, where = where)) {
      methods::removeClass(class, where)
    }
  }
}

globalVariables(c(
  ".Data",
  "className",
  "distance",
  "package",
  "prototype",
  "slots",
  "subClass",
  "superClass",
  "virtual"
))
