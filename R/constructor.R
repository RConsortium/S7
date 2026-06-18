new_constructor <- function(
  parent,
  properties,
  envir = asNamespace("S7"),
  package = NULL
) {
  properties <- as_properties(properties)

  if (identical(parent, S7_object) || (is_class(parent) && parent@abstract)) {
    # There's no parent constructor to delegate to, so the constructor must
    # handle all properties: inherited and newly declared (which win).
    all_props <- modify_list(
      attr(parent, "properties", exact = TRUE),
      properties
    )

    arg_info <- constructor_args(parent, all_props, envir, package)
    self_args <- as_names(names(arg_info$self), named = TRUE)

    new_object_call <-
      if (has_S7_symbols(envir, "new_object", "S7_object")) {
        bquote(new_object(S7_object(), ..(self_args)), splice = TRUE)
      } else {
        bquote(S7::new_object(S7::S7_object(), ..(self_args)), splice = TRUE)
      }

    return(new_function(
      args = arg_info$self,
      body = as.call(c(
        quote(`{`),
        # Force all promises here so that any errors are signaled from
        # the constructor() call instead of the new_object() call.
        unname(self_args),
        new_object_call
      )),
      env = envir
    ))
  }

  arg_info <- constructor_args(parent, properties, envir, package)
  self_args <- as_names(names(arg_info$self), named = TRUE)

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

  # Overridden properties replace the corresponding parent argument in place
  # so that positional matching is preserved.
  args <- modify_list(arg_info$parent, arg_info$self)

  override_info <- constructor_override_info(
    properties,
    attr(parent, "properties", exact = TRUE) %||% list()
  )

  # Read-only overrides are computed from the object, so don't expose them
  # as constructor arguments.
  if (length(override_info$omit_from_constructor) > 0) {
    args <- args[setdiff(names2(args), override_info$omit_from_constructor)]
  }

  # ensure default value for `...` is empty
  if ("..." %in% names(args)) {
    args[names(args) == "..."] <- list(quote(expr = ))
  }

  # Overridden properties generally go to both parent and child: the parent
  # receives required/default values and validates them, then new_object()
  # applies the child default/setter. Some overrides can't be forwarded because
  # the parent constructor can't accept them.
  parent_arg_nms <- constructor_parent_arg_names(
    names(arg_info$parent),
    override_info
  )
  parent_args <- as_names(parent_arg_nms, named = TRUE)
  names(parent_args)[names(parent_args) == "..."] <- ""
  parent_call <- new_call(parent_name, parent_args)
  body <- new_call(
    if (has_S7_symbols(envir, "new_object")) {
      "new_object"
    } else {
      c("S7", "new_object")
    },
    c(parent_call, self_args)
  )

  env <- new.env(parent = envir)
  env[[parent_name]] <- parent_fun

  new_function(args, body, env)
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

constructor_override_info <- function(properties, parent_props) {
  prop_nms <- names2(properties)
  override_nms <- intersect(prop_nms, names2(parent_props))

  read_only_override_nms <- intersect(
    prop_nms[vlapply(properties, prop_is_read_only)],
    override_nms
  )

  # Dynamic settable overrides can accept values the parent would reject unless
  # the child property narrows the parent property class.
  dynamic_settable_override_nms <- intersect(
    prop_nms[
      vlapply(properties, prop_is_dynamic) &
        vlapply(properties, prop_has_setter)
    ],
    override_nms
  )
  dynamic_settable_compatible <- vlapply(
    dynamic_settable_override_nms,
    function(name) {
      class_extends(properties[[name]]$class, parent_props[[name]]$class)
    }
  )
  incompatible_dynamic_settable_override_nms <-
    dynamic_settable_override_nms[!dynamic_settable_compatible]

  omit_from_parent <- c(
    read_only_override_nms,
    incompatible_dynamic_settable_override_nms
  )

  list(
    omit_from_constructor = read_only_override_nms,
    omit_from_parent = omit_from_parent,
    forward_to_parent = setdiff(override_nms, omit_from_parent)
  )
}

constructor_parent_arg_names <- function(parent_arg_nms, override_info) {
  out <- setdiff(parent_arg_nms, override_info$omit_from_parent)

  if ("..." %in% parent_arg_nms) {
    out <- union(out, override_info$forward_to_parent)
  }

  out
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

as_names <- function(x, named = FALSE) {
  if (named) {
    names(x) <- x
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
