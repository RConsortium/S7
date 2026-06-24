new_constructor <- function(
  parent,
  properties,
  envir = asNamespace("S7"),
  package = NULL
) {
  properties <- as_properties(properties)

  if (is_S4_class(parent) && !parent@virtual) {
    return(new_S4_constructor(parent, properties, envir, package))
  }

  if (
    identical(parent, S7_object) ||
      is_S4_class(parent) ||
      (is_class(parent) && parent@abstract)
  ) {
    # There's no parent constructor to delegate to, so the constructor must
    # handle all properties: inherited and newly declared (which win).
    parent_props <- if (is_S4_class(parent)) {
      class_properties(parent)
    } else {
      attr(parent, "properties", exact = TRUE)
    }
    all_props <- modify_list(
      parent_props,
      properties
    )

    arg_info <- constructor_args(parent, all_props, envir, package)
    force_args <- as_names(names(arg_info$self))

    s4_data_part <- is_S4_class(parent) && ".Data" %in% names(parent@slots)
    self_arg_names <- names(arg_info$self)
    parent_call <- if (s4_data_part) {
      self_arg_names <- setdiff(self_arg_names, ".Data")
      quote(.Data)
    } else if (has_S7_symbols(envir, "S7_object")) {
      quote(S7_object())
    } else {
      quote(S7::S7_object())
    }
    self_args <- as_names(self_arg_names)
    new_object_call <-
      if (has_S7_symbols(envir, "new_object")) {
        bquote(new_object(.(parent_call), ..(self_args)), splice = TRUE)
      } else {
        bquote(S7::new_object(.(parent_call), ..(self_args)), splice = TRUE)
      }

    return(new_function(
      args = arg_info$self,
      body = as.call(c(
        quote(`{`),
        # Force all promises here so that any errors are signaled from
        # the constructor() call instead of the new_object() call.
        unname(force_args),
        new_object_call
      )),
      env = envir
    ))
  }

  # We need a name so we get a compact constructor, and the actual function
  # which we'll embed in the constructor's environment
  if (is_class(parent)) {
    parent_name <- parent@name
    parent_fun <- parent
  } else if (is_base_class(parent)) {
    parent_name <- parent$constructor_name
    parent_fun <- parent$constructor
  } else if (is_S3_class(parent)) {
    parent_name <- paste0("new_", parent$class[[1]])
    parent_fun <- parent$constructor
  } else {
    # user facing error in S7_class()
    stop2("Unsupported `parent` type.", call = NULL)
  }
  parent_props <- attr(parent, "properties", exact = TRUE) %||% list()

  # We need to work out three things:
  # * The argument list for the constructor (`constr_args`)
  # * Which of those arguments is passed to the parent (`parent_args`)
  # * Which of those arguments is passed to new_object() (`self_args`)

  # In constructor args, the subclass default replaces the parent default
  arg_info <- constructor_args(parent, properties, envir, package)
  constr_args <- modify_list(arg_info$parent, arg_info$self)

  # The rest of the work is about moving args around not changing their values
  # so it's easier to work with their names
  constr_nms <- names2(constr_args)
  self_nms <- names2(arg_info$self)
  parent_nms <- names2(arg_info$parent)
  # We also need to figure out properties are overridden in the child
  override_nms <- intersect(constr_nms, names2(parent_props))

  # For overridden properties, we generally need to pass to both the parent
  # and the child so that we can both override parent defaults and respect
  # child setters. BUT we can't forward properties that are read-only in the
  # parent
  read_only_nms <- parent_nms[vlapply(properties, prop_is_read_only)]
  read_only_override_nms <- intersect(read_only_nms, override_nms)
  parent_nms <- setdiff(parent_nms, read_only_override_nms)
  constr_nms <- setdiff(constr_nms, read_only_override_nms)
  override_nms <- setdiff(override_nms, read_only_override_nms)

  # If parent takes ..., we can't match overrides by name, so pass all args on
  if ("..." %in% names(arg_info$parent)) {
    parent_nms <- union(parent_nms, override_nms)
  }

  # Now we can generate the parent and child calls
  parent_call <- new_call(parent_name, as_names(parent_nms))
  new_object <- c(if (!has_S7_symbols(envir, "new_object")) "S7", "new_object")
  child_call <- new_call(new_object, c(parent_call, as_names(self_nms)))

  # And finally the constructor itself
  env <- new.env(parent = envir)
  env[[parent_name]] <- parent_fun
  new_function(constr_args[constr_nms], child_call, env)
}

new_S4_constructor <- function(parent, properties, envir, package) {
  parent_props <- class_properties(parent)
  parent_nms <- names2(parent_props)
  override_nms <- intersect(names2(properties), parent_nms)
  self_props <- properties[setdiff(names2(properties), parent_nms)]

  parent_args <- as.pairlist(lapply(
    setNames(, parent_nms),
    function(name) {
      if (name %in% override_nms) {
        prop_default(properties[[name]], envir, package)
      } else {
        quote(expr = )
      }
    }
  ))
  self_args <- constructor_args(S7_object, self_props, envir, package)$self
  constr_args <- modify_list(parent_args, self_args)

  parent_arg_exprs <- lapply(parent_nms, function(name) {
    value <- as.name(name)
    if (name %in% override_nms) {
      bquote(.parent_args[.(name)] <- list(.(value)))
    } else {
      bquote(if (!missing(.(value))) .parent_args[.(name)] <- list(.(value)))
    }
  })

  parent_value_nms <- setdiff(parent_nms, ".Data")
  parent_value_args <- lapply(parent_value_nms, function(name) {
    bquote(.parent_values[[.(name)]])
  })
  names(parent_value_args) <- parent_value_nms

  self_value_args <- as_names(names2(self_args))
  parent_seed <- if (".Data" %in% parent_nms) {
    quote(.parent_values[[".Data"]])
  } else {
    quote(.S7_object())
  }
  new_object_call <- as.call(c(
    list(quote(.S7_new_object), parent_seed),
    parent_value_args,
    self_value_args
  ))

  env <- new.env(parent = envir)
  env$.S4_parent <- class_constructor(parent)
  env$.S4_initialize_values <- S4_initialize_values
  env$.S7_new_object <- new_object
  env$.S7_object <- S7_object

  new_function(
    constr_args,
    as.call(c(
      quote(`{`),
      quote(.parent_args <- list()),
      parent_arg_exprs,
      list(
        quote(.parent <- do.call(.S4_parent, .parent_args)),
        quote(.parent_values <- .S4_initialize_values(.parent)),
        new_object_call
      )
    )),
    env
  )
}

constructor_args <- function(
  parent,
  properties = list(),
  envir = asNamespace("S7"),
  package = NULL
) {
  parent_args <- formals(class_constructor(parent))

  # Remove read-only properties
  properties <- properties[!vlapply(properties, prop_is_read_only)]

  self_args <- as.pairlist(lapply(
    setNames(, names2(properties)),
    function(name) prop_default(properties[[name]], envir, package)
  ))

  list(parent = parent_args, self = self_args)
}

# helpers -----------------------------------------------------------------

is_property_dynamic <- function(x) is.function(x$getter)

missing_args <- function(names) {
  lapply(setNames(, names), function(i) quote(class_missing))
}

new_call <- function(call, args) {
  if (is.character(call)) {
    call <- switch(
      length(call),
      as.name(call),
      as.call(c(quote(`::`), lapply(call, as.name)))
    )
  }
  as.call(c(list(call), args))
}

as_names <- function(x) {
  if (length(x) > 0) {
    names(x) <- ifelse(x == "...", "", x)
  }
  lapply(x, as.name)
}

has_S7_symbols <- function(env, ...) {
  env <- topenv(env)
  if (identical(env, asNamespace("S7"))) {
    return(TRUE)
  }
  if (!isNamespace(env)) {
    return(FALSE)
  }
  imports <- getNamespaceImports(env)[["S7"]]
  symbols <- c(...) %||% getNamespaceExports("S7")
  all(symbols %in% imports)
}
