#' Register an S7 or S3 class with S4
#'
#' If you want to use [method<-] to register a method for an S4 generic with
#' an S7 class that does not extend an S4 class, or an S3 class created by
#' [new_S3_class()], you need to call `S4_register()` once. Classes created by
#' [new_class()] with an S4 parent are registered automatically.
#'
#' @param class An S7 class created with [new_class()], or an S3 class created
#'   with [new_S3_class()].
#' @param env Expert use only. Environment where S4 class will be registered.
#' @returns Nothing; the function is called for its side-effect.
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
S4_register <- function(class, env = parent.frame()) {
  where <- topenv(env)

  if (is_S3_class(class)) {
    methods::setOldClass(class$class, where = where)
    return(invisible())
  }

  if (!is_class(class)) {
    msg <- sprintf(
      "`class` must be an S7 class or an S3 class, not a %s.",
      obj_desc(class)
    )
    stop2(msg)
  }

  classes <- class_dispatch(class)
  if (is_S4_class(class@parent)) {
    methods::setOldClass(
      classes,
      S4Class = S4_transient_prototype_class(class, env),
      where = where
    )
  } else {
    methods::setOldClass(classes, where = where)
  }
  methods::setValidity(classes[1], S4_validate, where = where)
  methods::setMethod("initialize", classes[1], S4_initialize, where = where)
  invisible()
}

S4_needs_registration <- function(class) {
  parent <- attr(class, "parent", exact = TRUE)
  is_S4_class(parent) ||
    (is_class(parent) && S4_needs_registration(parent))
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

S4_transient_prototype_class <- function(class, env = parent.frame()) {
  where <- topenv(env)
  classes <- class_dispatch(class)

  args <- list(
    Class = classes[1L],
    contains = class@parent@className,
    where = where
  )

  if (!is.null(class@package)) {
    args$package <- class@package
  }

  do.call(methods::setClass, args)
  methods::getClass(args$Class, where = where)
}

is_S4_class <- function(x) inherits(x, "classRepresentation")

S4_to_S7_class <- function(x, error_base = "", call = sys.call(-1L)) {
  # Silence R CMD check false positives
  distance <- subClass <- className <- package <- NULL

  # Convert generator function to class
  if (methods::is(x, "classGeneratorFunction")) {
    return(S4_to_S7_class(
      methods::getClass(as.character(x@className)),
      error_base,
      call = call
    ))
  }

  if (methods::is(x, "ClassUnionRepresentation")) {
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
    if (methods::extends(x, "oldClass")) {
      new_S3_class(as.character(x@className))
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
  x@virtual && methods::extends(x, "oldClass") && x@className != "oldClass"
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
  if (methods::getPackageName(topenv(env)) == f@package) {
    ## current ns might not be loaded yet, catch here
    return(f@package)
  }

  name <- as.vector(f@generic)
  generic_in_its_package <- methods::isGeneric(
    name,
    where = asNamespace(f@package)
  )
  if (generic_in_its_package) {
    return(f@package)
  }

  # generic was defined for a function from a different package, like base
  find_package_with_symbol(name, env, exclude = f@package) %||%
    stop(
      "Failed to find originating package for S4 generic '",
      name,
      "' in namespace imports.",
      call. = FALSE
    )
}

find_package_with_symbol <- function(name, env, exclude = NULL) {
  imports <- getNamespaceImports(topenv(env))
  pkgs <- setdiff(names(imports), exclude)
  Find(
    function(pkg) {
      if (isTRUE(imports[[pkg]])) {
        name %in% getNamespaceExports(pkg)
      } else {
        name %in% imports[[pkg]]
      }
    },
    pkgs
  )
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
