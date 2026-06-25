#' Register an S7, S3, or union class with S4
#'
#' If you want to use [method<-] to register a method for an S4 generic with
#' an S7 class that does not extend an S4 class, or an S3 class created by
#' [new_S3_class()], you need to call `S4_register()` once. Classes created by
#' [new_class()] with an S4 parent are registered automatically.
#'
#' @section Details:
#' `S4_register()` registers an S7 class, S3 class, or S7 union with S4 and
#' invisibly returns the registered S4 class name.
#' For S7 classes, this creates a virtual S4 old class that exposes stored S7
#' properties as S4 slots and carries the `_S7_class` slot needed for S7 dispatch
#' and validation.
#'
#' After registration, `S4_contains()` returns the virtual S4 class name to use
#' when an S4 class should extend an S7 class with
#' `methods::setClass(contains = )`. It asserts that the S7 properties can
#' safely cross the S4 inheritance boundary.
#'
#' Register S7 unions with `S4_register()` before using them in S4 slots or
#' method signatures unless an equivalent S4 union already exists.
#'
#' See `vignette("compatibility")` for examples and caveats.
#'
#' @param class An S7 class created with [new_class()], or, for
#'   `S4_register()` only, an S3 class created with [new_S3_class()] or an S7
#'   union created with [new_union()].
#' @param env Expert use only. Environment where S4 class will be registered.
#' @returns
#' Called for its side effects and invisibly returns the registered S4 class
#' name.
#' @export
#' @examples
#' methods::setGeneric("S4_generic", function(x) {
#'   standardGeneric("S4_generic")
#' })
#'
#' Foo := new_class()
#' S4_register(Foo)
#' method(S4_generic, Foo) <- function(x) "Hello"
#'
#' S4_generic(Foo())
#'
#' S4Foo := new_class(properties = list(x = class_numeric), package = "S7")
#' S4_register(S4Foo)
#' methods::setClass("S4Child", contains = S4_contains(S4Foo))
S4_register <- function(class, env = parent.frame()) {
  where <- topenv(env)
  if (is_union(class)) {
    return(invisible(S4_register_union(class, where)))
  } else if (is_class(class)) {
    return(invisible(S4_register_class(class, where)))
  } else if (is_S3_class(class)) {
    classes <- class$class
    methods::setOldClass(classes, where = where)
    return(invisible(classes[1L]))
  } else {
    msg <- sprintf(
      "`class` must be an S7 class, S3 class, or S7 union, not a %s.",
      obj_desc(class)
    )
    stop2(msg)
  }
}

#' @rdname S4_register
#' @export
S4_contains <- function(class, env = parent.frame()) {
  where <- topenv(env)
  if (!is_class(class)) {
    msg <- sprintf("`class` must be an S7 class, not a %s.", obj_desc(class))
    stop2(msg)
  }
  if (class@abstract) {
    stop2("S4 classes can not extend abstract S7 classes.", call = sys.call())
  }

  class_name <- as.character(
    S4_registered_class(class, where, call = sys.call())@className
  )
  S4_check_contains(class)
  class_name
}

S4_register_union <- function(class, env) {
  members <- S4_union_member_classes(class, env, register = TRUE)
  name <- S4_union_name(class, env)
  methods::setClassUnion(
    name,
    members,
    where = env
  )
  S4_register_union_member_extensions(class$classes, members, name, env)
  name
}

S4_class <- function(x, S4_env, call = sys.call(-1L)) {
  switch(
    class_type(x),
    `NULL` = "NULL",
    missing = "missing",
    any = "ANY",
    S7_base = base_to_S4(x$class),
    S4 = x@className,
    S7 = S4_registered_class(x, S4_env, call = call)@className,
    S7_S3 = S4_registered_class(x, S4_env, call = call)@className,
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
  class <- S4_registered_class_or_null(x, S4_env)
  if (!is.null(class)) {
    return(class)
  }

  msg <- sprintf(
    "Class has not been registered with S4; please call S4_register(%s).",
    class_deparse(x)
  )
  stop2(msg, call = call)
}

S4_registered_class_or_null <- function(x, S4_env) {
  class <- tryCatch(
    methods::getClass(class_register(x), where = S4_env),
    error = function(err) NULL
  )
  if (is.null(class) || !S4_registered_class_matches(class, x)) {
    return(NULL)
  }
  class
}

S4_registered_class_matches <- function(class, x) {
  if (S4_registered_S7_class_matches(class, x)) {
    TRUE
  } else if (!"_S7_class" %in% names(class@slots)) {
    S4_registered_old_class_matches(class, x)
  } else {
    FALSE
  }
}

S4_registered_S7_class_matches <- function(class, x) {
  if (!"_S7_class" %in% names(class@slots)) {
    return(FALSE)
  }
  registered <- methods::slot(class@prototype, "_S7_class")
  is_class(registered) && S4_registered_S7_class_object_matches(registered, x)
}

S4_registered_S7_class_object_matches <- function(registered, x) {
  identical(registered, x) || isTRUE(all.equal(registered, x))
}

S4_registered_old_class_matches <- function(class, x) {
  if (!".S3Class" %in% names(class@slots)) {
    return(FALSE)
  }
  classes <- if (is_S3_class(x)) x$class else S4_reified_old_classes(x)
  identical(
    methods::slot(class@prototype, ".S3Class"),
    classes
  )
}

S4_union_class <- function(x, S4_env) {
  if (identical(x, class_numeric)) {
    return("numeric")
  }

  name <- S4_union_name(x, S4_env)
  members <- S4_union_member_classes(x, S4_env)
  if (
    methods::isClass(name, where = S4_env) &&
      S4_union_matches(name, members, S4_env)
  ) {
    return(name)
  }

  name <- S4_find_union(members, S4_env)
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
  paste0(
    vcapply(x$classes, S4_union_name_member, S4_env = S4_env),
    collapse = "_OR_"
  )
}

S4_union_name_member <- function(x, S4_env) {
  if (is_S4_class(x)) {
    return(S4_class_key(x@className))
  }

  S4_class(x, S4_env)
}

S4_union_member_classes <- function(x, S4_env, register = FALSE) {
  lapply(
    x$classes,
    S4_union_member_class,
    S4_env = S4_env,
    register = register
  )
}

S4_union_member_class <- function(x, S4_env, register = FALSE) {
  if (!is_S4_class(x)) {
    if (register && class_type(x) %in% c("S7", "S7_S3")) {
      registered <- S4_registered_class_or_null(x, S4_env)
      if (!is.null(registered)) {
        return(registered@className)
      }
      return(S4_register(x, S4_env))
    }
    return(S4_class(x, S4_env))
  }

  if (!S4_union_member_needs_identity(x@className)) {
    return(x@className)
  }

  name <- S4_class_name(x)
  if (register) {
    if (!methods::isClass(name, where = S4_env)) {
      methods::setClass(name, contains = "VIRTUAL", where = S4_env)
    }
    return(name)
  }
  if (methods::isClass(name, where = S4_env)) {
    return(name)
  }

  x@className
}

S4_register_union_member_extensions <- function(
  classes,
  members,
  union,
  S4_env
) {
  for (i in seq_along(classes)) {
    S4_register_union_member_extension(
      classes[[i]],
      members[[i]],
      union,
      S4_env
    )
  }
  invisible()
}

S4_register_union_member_extension <- function(class, member, union, S4_env) {
  if (!is_S4_class(class) || !S4_union_member_needs_identity(class@className)) {
    return(invisible())
  }

  member_def <- methods::getClass(member, where = S4_env)
  subclasses <- base::Filter(function(x) x@distance == 1, member_def@subclasses)
  if (
    S4_class_key(class@className) %in%
      S4_class_keys(S4_extension_subclasses(subclasses))
  ) {
    return(invisible())
  }

  setIs_args <- list(
    class@className,
    member,
    where = S4_env,
    classDef = class
  )
  if (!S4_union_member_same_package(class@className, S4_env)) {
    setIs_args$test <- S4_class_identity_test(class@className)
  }
  suppressWarnings(do.call(methods::setIs, setIs_args))
  S4_prune_union_subclass(union, class@className, S4_env)
  invisible()
}

S4_union_member_needs_identity <- function(class) {
  package <- attr(class, "package", exact = TRUE)
  !is.null(package) &&
    !(identical(package, "methods") && class %in% S4_methods_class_names())
}

S4_union_member_same_package <- function(class, S4_env) {
  package <- attr(class, "package", exact = TRUE)
  !is.null(package) && identical(package, methods::getPackageName(S4_env))
}

# setIs() adds a transitive bare-class subclass entry to the union. Class
# unions match those names without respecting package identity, so remove it
# and let objects reach the union through the package-qualified shim.
S4_prune_union_subclass <- function(union, class, S4_env) {
  class <- as.character(class)
  union_def <- methods::getClass(union, where = S4_env)
  if (!class %in% names(union_def@subclasses)) {
    return(invisible())
  }

  union_def@subclasses[[class]] <- NULL
  methods::setClass(Class = union, representation = union_def, where = S4_env)
  invisible()
}

S4_class_identity_test <- function(target) {
  target <- S4_class_identity_expr(target)
  new_function(
    alist(object = ),
    bquote({
      class <- S4_object_class_def(base::class(object))
      if (is.null(class)) {
        return(FALSE)
      }

      S4_class_extends_identity(class, .(target))
    })
  )
}

S4_class_identity_expr <- function(class) {
  package <- attr(class, "package", exact = TRUE)
  class <- as.character(class)
  if (is.null(package)) {
    return(class)
  }

  bquote(structure(.(class), package = .(package)))
}

S4_object_class_def <- function(class) {
  name <- class[[1L]]
  package <- attr(class, "package", exact = TRUE)
  if (!is.null(package)) {
    where <- if (identical(package, ".GlobalEnv")) {
      .GlobalEnv
    } else {
      asNamespace(package)
    }
    return(methods::getClass(name, where = where))
  }

  methods::getClassDef(name, inherits = FALSE)
}

S4_class_extends_identity <- function(class, target) {
  if (S4_same_class_identity(class@className, target)) {
    return(TRUE)
  }

  any(vlapply(
    class@contains,
    function(extension) S4_same_class_identity(extension@superClass, target)
  ))
}

S4_same_class_identity <- function(x, y) {
  identical(S4_class_identity_key(x), S4_class_identity_key(y))
}

S4_class_identity_key <- function(class) {
  package <- attr(class, "package", exact = TRUE)
  if (is.null(package) && class %in% S4_methods_class_names()) {
    package <- "methods"
  }

  paste0(package %||% "", "::", as.character(class))
}

S4_find_union <- function(members, S4_env) {
  if (!all(vlapply(members, methods::isClass, where = S4_env))) {
    return(NULL)
  }

  supers <- lapply(members, S4_direct_superclasses, S4_env = S4_env)
  super_keys <- lapply(supers, S4_class_keys)
  candidate_keys <- base::Reduce(base::intersect, super_keys)
  candidates <- supers[[1L]][match(candidate_keys, super_keys[[1L]])]
  matches <- base::Filter(
    function(candidate) S4_union_matches(candidate, members, S4_env),
    candidates
  )
  if (length(matches) == 0) {
    return(NULL)
  }

  matches[[1L]]
}

S4_direct_superclasses <- function(class, S4_env) {
  class <- methods::getClass(class, where = S4_env)
  contains <- class@contains
  contains <- base::Filter(function(x) x@distance == 1, contains)
  lapply(contains, function(x) x@superClass)
}

S4_union_matches <- function(class, members, S4_env) {
  class <- methods::getClass(class, where = S4_env)
  if (!methods::isClassUnion(class)) {
    return(FALSE)
  }

  setequal(S4_class_keys(S4_union_members(class)), S4_class_keys(members))
}

S4_union_members <- function(class) {
  subclasses <- base::Filter(function(x) x@distance == 1, class@subclasses)
  S4_extension_subclasses(subclasses)
}

S4_extension_subclasses <- function(x) {
  lapply(x, function(x) x@subClass)
}

S4_class_keys <- function(classes) {
  vcapply(classes, S4_class_key)
}

S4_class_key <- function(class) {
  package <- attr(class, "package", exact = TRUE)
  class <- as.character(class)
  if (is.null(package) && class %in% S4_methods_class_names()) {
    package <- "methods"
  }
  if (is.null(package) || identical(package, ".GlobalEnv")) {
    class
  } else {
    paste0(package, "::", class)
  }
}

S4_methods_class_names <- function() {
  c("NULL", "missing", "ANY", names(S4_basic_classes()))
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
  S4_register(class, env)
  invisible()
}

S4_set_S3_class_prototype <- function(class, S3_class, env) {
  class_def <- methods::getClass(class, where = env)
  attr(class_def@prototype, ".S3Class") <- S3_class
  methods::setClass(Class = class, representation = class_def, where = env)
  invisible(class)
}

S4_slot_names <- function(class, S4_env) {
  names(methods::getClass(class, where = S4_env)@slots)
}

S4_internal_slot_names <- function() {
  c("_S7_class", ".S3Class")
}

S4_property_class <- function(prop, S4_env) {
  S4_register_property_class(prop$class, S4_env)
  S4_class(prop$class, S4_env)
}

S4_register_property_class <- function(class, S4_env) {
  if (
    is_class(class) && !methods::isClass(class_register(class), where = S4_env)
  ) {
    S4_register(class, S4_env)
  }
  invisible()
}

S4_check_contains <- function(class, call = sys.call(-1L)) {
  if (!is_class(class)) {
    return(invisible())
  }

  for (prop in class@properties) {
    if (prop_is_dynamic(prop) || prop_has_setter(prop)) {
      msg <- sprintf(
        paste0(
          "Can't extend S7 class %s with S4 because property %s has a ",
          "custom %s."
        ),
        class_desc(class),
        prop$name,
        if (prop_is_dynamic(prop)) "getter" else "setter"
      )
      stop2(msg, call = call)
    }
  }

  invisible()
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
      slot_name <- prop_storage_rename(name)
      args[slot_name] <- value
    }
  }
  if (include_S7_class) {
    args$`_S7_class` <- class
  }
  do.call(methods::prototype, args)
}

S4_property_prototype <- function(prop, env, package) {
  tryCatch(
    {
      value <- prop_default(prop, env, package)
      value <- S4_decode_pseudo_null(value)
      if (is.call(value) || is.symbol(value)) {
        return(list())
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

S4_property_deferred_default <- function(
  prop,
  env,
  package,
  allow_simple = FALSE
) {
  tryCatch(
    {
      value <- prop_default(prop, env, package)
      value <- S4_decode_pseudo_null(value)
      if (!is.call(value) && !is.symbol(value)) {
        if (allow_simple) {
          return(list(value))
        }
        return(list())
      }
      value <- eval(value, env)
      value <- S4_decode_pseudo_null(value)
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

S4_decode_pseudo_null <- function(value) {
  if (identical(value, as.name("\001NULL\001"))) NULL else value
}

S4_old_classes <- function(class) {
  subclasses <- character()
  while (is_class(class)) {
    if (class@abstract) {
      return(subclasses)
    }
    subclasses <- c(subclasses, S7_class_name(class))
    class <- class@parent
    if (is_S4_class(class)) {
      return(subclasses)
    }
  }
  character()
}

S4_register_class <- function(class, env = parent.frame()) {
  where <- topenv(env)
  class_name <- S7_class_name(class)
  parent_class_name <- S4_reified_parent_class(class, where)
  old_classes <- S4_reified_old_classes(class)
  parent_slot_names <- character()
  if (!is.null(parent_class_name)) {
    parent_slot_names <- S4_slot_names(parent_class_name, where)
  }
  parent_needs_identity <- S4_class_needs_identity(parent_class_name, where)

  properties <- class@properties
  stored_properties <- properties[!vlapply(properties, prop_is_dynamic)]
  prototype_properties <- stored_properties[setdiff(
    names(stored_properties),
    ".Data"
  )]
  slot_properties <- stored_properties
  if (!".Data" %in% parent_slot_names) {
    slot_properties <- slot_properties[setdiff(names(slot_properties), ".Data")]
  }
  if (!parent_needs_identity) {
    slot_properties <- slot_properties[
      !prop_storage_rename(names(slot_properties)) %in% parent_slot_names
    ]
  }
  slots <- lapply(slot_properties, S4_property_class, S4_env = where)
  names(slots) <- prop_storage_rename(names(slot_properties))
  needs_S7_class_slot <- !"_S7_class" %in% parent_slot_names
  if (needs_S7_class_slot) {
    slots$`_S7_class` <- "S7_class"
  }
  if (S4_needs_S3_class_slot(class, parent_slot_names, old_classes)) {
    slots$.S3Class <- "character"
  }
  contains <- S4_contains_classes(parent_class_name, where)

  methods::setClass(
    Class = class_name,
    slots = slots,
    contains = contains,
    prototype = S4_properties_prototype(
      prototype_properties,
      class,
      where,
      include_S7_class = TRUE
    ),
    where = where
  )

  if (class@abstract) {
    if (!S7_extends_S4(class)) {
      methods::setOldClass(old_classes, S4Class = class_name, where = where)
      S4_set_S7_object_extension(class_name, old_classes, where)
      S4_set_S3_class_prototype(class_name, old_classes, where)
    }
    return(class_name)
  }

  methods::setOldClass(old_classes, S4Class = class_name, where = where)
  S4_set_S7_object_extension(class_name, old_classes, where)
  S4_set_S3_class_prototype(class_name, old_classes, where)
  methods::setValidity(class_name, S4_validate_class, where = where)
  methods::setMethod(
    "initialize",
    class_name,
    S4_initialize_method(where),
    where = where
  )

  class_name
}

S4_needs_S3_class_slot <- function(class, parent_slot_names, old_classes) {
  if (!"S7_object" %in% old_classes || ".S3Class" %in% parent_slot_names) {
    return(FALSE)
  }

  is_base_class(base_parent(class)) || ".Data" %in% parent_slot_names
}

S4_set_S7_object_extension <- function(class_name, old_classes, where) {
  if (!"S7_object" %in% old_classes) {
    return(invisible())
  }

  class <- methods::getClass(class_name, where = where)
  if (methods::extends(class, "S7_object")) {
    return(invisible())
  }

  methods::setIs(
    class_name,
    "S7_object",
    where = where,
    classDef = class
  )
  invisible()
}

S4_class_needs_identity <- function(class, where) {
  if (is.null(class)) {
    return(FALSE)
  }

  package <- attr(class, "package", exact = TRUE)
  !is.null(package) && !identical(package, methods::getPackageName(where))
}

S4_contains_classes <- function(parent_class_name, where) {
  if (S4_class_needs_identity(parent_class_name, where)) {
    list(parent_class_name, "VIRTUAL")
  } else {
    c(parent_class_name, "VIRTUAL")
  }
}

S4_check_slot_storage <- function(class, call = sys.call(-1L)) {
  if (S4_has_implicit_data_part(class)) {
    msg <- c(
      sprintf(
        "Can't extend S4 class %s because it has an implicit data part.",
        class_desc(class)
      ),
      "Only S4 classes with data parts stored in an explicit `.Data` slot are supported."
    )
    stop2(msg, call = call)
  }

  nms <- names(class@slots)
  renamed <- nms[prop_storage_rename(nms) != nms & nms != ".Data"]
  if (length(renamed) == 0L) {
    return(invisible())
  }

  renamed_label <- paste(dQuote(renamed), collapse = ", ")
  slot_label <- if (length(renamed) == 1L) "slot" else "slots"
  msg <- c(
    sprintf(
      "Can't extend S4 class %s because %s %s would need renamed S7 storage.",
      class_desc(class),
      slot_label,
      renamed_label
    ),
    "These S4 slots can not be represented safely on direct S7 child objects."
  )
  stop2(msg, call = call)
}

S4_has_implicit_data_part <- function(class) {
  if (".Data" %in% names(class@slots)) {
    return(FALSE)
  }

  identical(as.character(class@className), "vector") ||
    "vector" %in% names(class@contains)
}

S4_reified_parent_class <- function(class, env) {
  parent_class <- class@parent
  if (is_class(parent_class)) {
    if (parent_class@name == "S7_object") {
      return(NULL)
    }
    return(S4_register(parent_class, env))
  } else if (is_base_class(parent_class)) {
    return(S4_class(parent_class, env))
  } else if (is_S4_class(parent_class)) {
    return(S4_class(parent_class, env))
  }

  NULL
}

S4_reified_old_classes <- function(class) {
  if (S7_extends_S4(class)) {
    return(c(S4_old_classes(class), "S7_object"))
  }

  class_dispatch(class)
}

S4_validate_class <- function(object) {
  class_name <- class(object)[1L]
  class <- S7_class(object)

  if (identical(S7_class_name(class), class_name)) {
    return(S4_validate(object))
  }
  while (is_class(class@parent)) {
    class <- class@parent
    if (identical(S7_class_name(class), class_name)) {
      return(TRUE)
    }
  }

  sprintf(
    "object with S7 class %s does not match S4 class %s",
    dQuote(S7_class_name(S7_class(object))),
    dQuote(class_name)
  )
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

S4_initialize_method <- function(env) {
  force(env)
  function(.Object, ...) {
    S4_initialize(.Object, ..., .S4_default_env = env)
  }
}

S4_initialize <- function(.Object, ..., .S4_default_env = parent.frame()) {
  if (isS4(.Object) && has_S7_class(.Object)) {
    S4_check_contains(S7_class(.Object))
  }

  args <- list(...)
  nms <- names2(args)
  prop_nms <- prop_names(.Object)
  vals <- list()
  s4_vals <- list()
  s4_slot_nms <- character()
  if (isS4(.Object)) {
    prop_storage_nms <- prop_storage_rename(prop_nms)
    s4_slot_nms <- setdiff(
      methods::slotNames(.Object),
      c(prop_nms, prop_storage_nms, S4_internal_slot_names())
    )
  }
  data_part <- NULL
  for (arg in args[nms == ""]) {
    arg_vals <- S4_initialize_values(arg)
    if (".Data" %in% names(arg_vals)) {
      data_part <- if (isS4(arg)) arg_vals$.Data else arg
    }
    if (isS4(arg)) {
      arg_s4_vals <- arg_vals[names(arg_vals) %in% s4_slot_nms]
      s4_vals <- modify_list(s4_vals, arg_s4_vals)
    }
    arg_vals <- S4_initialize_prop_values(
      arg_vals,
      prop_nms,
      storage = isS4(arg)
    )
    vals <- modify_list(vals, arg_vals)
  }
  named_args <- args[nms != ""]
  internal_arg_nms <- intersect(names2(named_args), S4_internal_slot_names())
  if (length(internal_arg_nms) > 0L) {
    slot_label <- if (length(internal_arg_nms) == 1L) "slot" else "slots"
    msg <- sprintf(
      "Can't initialize internal S4 %s %s.",
      slot_label,
      paste(dQuote(internal_arg_nms), collapse = ", ")
    )
    stop2(msg, call = sys.call(-1L))
  }
  if (length(s4_slot_nms) > 0L) {
    s4_vals <- modify_list(
      s4_vals,
      named_args[names(named_args) %in% s4_slot_nms]
    )
    named_args <- named_args[!names(named_args) %in% s4_slot_nms]
  }
  prop_storage_idx <- match(names(named_args), prop_storage_rename(prop_nms))
  names(named_args)[!is.na(prop_storage_idx)] <-
    prop_nms[prop_storage_idx[!is.na(prop_storage_idx)]]
  vals <- modify_list(vals, named_args)
  if (".Data" %in% names(named_args)) {
    data_part <- vals$.Data
  }

  if (!is.null(data_part)) {
    .Object <- S4_initialize_data_part(data_part, .Object)
  }

  vals <- modify_list(
    S4_initialize_default_values(.Object, names(vals), .S4_default_env),
    vals
  )
  if (".Data" %in% names(vals)) {
    .Object <- S4_initialize_data_part(vals$.Data, .Object)
    vals$.Data <- NULL
  }
  props(.Object, check = FALSE) <- vals
  for (name in names(s4_vals)) {
    methods::slot(.Object, name) <- s4_vals[[name]]
  }
  validate(.Object)
  .Object
}

S4_initialize_prop_values <- function(values, properties, storage = FALSE) {
  if (!storage) {
    return(values[names(values) %in% properties])
  }

  storage_nms <- prop_storage_rename(properties)
  idx <- match(names(values), storage_nms)
  values <- values[!is.na(idx)]
  names(values) <- properties[idx[!is.na(idx)]]
  values
}

S4_initialize_default_values <- function(object, supplied, S4_env) {
  class <- S7_class(object)
  env <- environment(class)
  properties <- class@properties
  properties <- properties[!vlapply(properties, prop_is_dynamic)]
  property_slot_nms <- prop_storage_rename(names(properties))
  properties <- properties[property_slot_nms %in% methods::slotNames(object)]
  properties <- properties[setdiff(names(properties), supplied)]

  values <- list()
  for (name in names(properties)) {
    slot_name <- prop_storage_rename(name)
    if (!S4_slot_has_prototype_value(object, slot_name, S4_env)) {
      next
    }

    prop <- properties[[name]]
    value <- S4_property_deferred_default(
      prop,
      env,
      class@package,
      allow_simple = identical(name, ".Data")
    )
    if (length(value) != 0L) {
      values[name] <- value
    }
  }
  values
}

S4_slot_has_prototype_value <- function(object, name, S4_env) {
  prototype <- S4_object_class(object, S4_env)@prototype
  value <- methods::slot(object, name)
  prototype_value <- methods::slot(prototype, name)
  if (identical(value, prototype_value)) {
    return(TRUE)
  }
  if (!identical(name, ".Data")) {
    return(FALSE)
  }

  identical(
    S4_strip_data_part_identity(value),
    S4_strip_data_part_identity(prototype_value)
  )
}

S4_object_class <- function(object, S4_env) {
  class <- class(object)
  name <- class[[1L]]
  package <- attr(class, "package", exact = TRUE)
  if (!is.null(package)) {
    class_def <- methods::getClassDef(
      name,
      package = package,
      inherits = FALSE
    )
    if (!is.null(class_def)) {
      return(class_def)
    }
  }

  methods::getClass(name, where = S4_env)
}

S4_strip_data_part_identity <- function(x) {
  attrs <- attributes(x)
  attrs[S4_internal_slot_names()] <- NULL
  attributes(x) <- attrs
  x
}

S4_initialize_values <- function(object) {
  if (isS4(object)) {
    slots <- methods::slotNames(object)
    values <- stats::setNames(
      lapply(slots, methods::slot, object = object),
      slots
    )
    if (is_S4_data_part_object(object)) {
      values$.Data <- S4_data_part(object)
    }
    values
  } else if (S7_inherits(object)) {
    props(object)
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
  protected <- S4_data_part_protected_attributes(object)
  incoming <- attributes(value) %||% list()
  incoming[protected] <- NULL
  if (isS4(object)) {
    data <- zap_attr(unclass(value), protected)
    methods::slot(object, ".Data") <- data
    attributes(object) <- modify_list(attributes(object), incoming)
    return(object)
  }

  value <- zap_attr(value, protected)
  attributes(value) <- modify_list(attributes(object), incoming)
  value
}

S4_data_part_protected_attributes <- function(object) {
  c(
    methods::slotNames(object),
    if (has_S7_class(object)) prop_storage_names(object),
    "class",
    "_S7_class",
    "S7_class"
  )
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
    if (S4_is_reified_S7_class(x)) {
      class <- methods::slot(x@prototype, "_S7_class")
      return(class)
    }
    if (is_S3_oldClass(x)) {
      new_S3_class(S3_oldClass_classes(x))
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
  slots <- class@slots
  slots <- slots[!names(slots) %in% S4_internal_slot_names()]
  properties <- Map(
    S4_slot_property,
    slots,
    names(slots),
    MoreArgs = list(owner = class)
  )
  names(properties) <- names(slots)
  properties
}

S4_slot_property <- function(class, name, owner) {
  new_property(
    class = S4_to_S7_class(methods::getClass(class)),
    default = S4_slot_prototype_default(owner, name),
    name = name
  )
}

S4_slot_prototype_default <- function(class, name) {
  if (identical(name, ".Data")) {
    return(bquote(
      methods::getClass(.(class@className))@prototype@.Data
    ))
  }

  bquote(
    attr(
      methods::getClass(.(class@className))@prototype,
      .(name),
      exact = TRUE
    )
  )
}

S4_basic_classes <- function() {
  list(
    NULL = NULL,
    ANY = class_any,
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

is_S3_oldClass <- function(x) {
  methods::extends(x, "oldClass") &&
    !"_S7_class" %in% names(x@slots) &&
    (is_oldClass(x) || is_ordered_oldClass(x))
}

is_ordered_oldClass <- function(x) {
  identical(as.character(x@className), "ordered") &&
    identical(attr(x@prototype, ".S3Class"), "factor")
}

S3_oldClass_classes <- function(x) {
  class <- attr(x@prototype, ".S3Class")
  if (!x@className %in% class) {
    class <- c(as.character(x@className), class)
  }
  class
}

S4_class_name <- function(x) {
  if (is_oldClass(x)) {
    return(x@className)
  }
  S7_class <- S4_reified_S7_class(x)
  if (!is.null(S7_class)) {
    return(S7_class_name(S7_class))
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

S4_reified_S7_class <- function(x) {
  if (!"_S7_class" %in% names(x@slots)) {
    return(NULL)
  }

  class <- methods::slot(x@prototype, "_S7_class")
  if (is_class(class) && class@abstract) class
}

S4_is_reified_S7_class <- function(x) {
  if (!"_S7_class" %in% names(x@slots)) {
    return(FALSE)
  }

  class <- methods::slot(x@prototype, "_S7_class")
  is_class(class) && identical(as.character(x@className), S7_class_name(class))
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
  "subclasses",
  "subClass",
  "superClass",
  "virtual"
))
