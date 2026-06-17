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

  parent_prop_nms <- names2(attr(parent, "properties", exact = TRUE) %||% list())
  prop_nms <- names2(properties)

  # Read-only props that override parent props don't get args
  is_read_only <- vlapply(properties, prop_is_read_only)
  read_only_override_nms <- intersect(prop_nms[is_read_only], parent_prop_nms)
  if (length(read_only_override_nms) > 0) {
    args <- args[setdiff(names2(args), read_only_override_nms)]
  }

  # ensure default value for `...` is empty
  if ("..." %in% names(args)) {
    args[names(args) == "..."] <- list(quote(expr = ))
  }

  # Overridden properties are passed to both the parent and child.
  # This makes it possible to override mandatory properties and custom setters.
  is_dynamic_settable <- vlapply(properties, prop_is_dynamic) &
    vlapply(properties, prop_has_setter)
  dynamic_settable_override_nms <- intersect(
    prop_nms[is_dynamic_settable],
    parent_prop_nms
  )
  parent_arg_nms <- setdiff(
    names(arg_info$parent),
    c(read_only_override_nms, dynamic_settable_override_nms)
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
