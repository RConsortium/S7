new_constructor <- function(
  parent,
  properties,
  envir = asNamespace("S7"),
  package = NULL
) {
  properties <- as_properties(properties)

  if (identical(parent, S7_object) || (is_class(parent) && parent@abstract)) {
    # Abstract parent: child must handle all properties since there's no
    # parent constructor to delegate to. Combine inherited properties with
    # any new/overridden child properties (child wins on conflicts).
    abstract_props <- if (is_class(parent)) parent@properties else list()
    abstract_props[names(properties)] <- properties
    arg_info <- constructor_args(parent, abstract_props, envir, package)
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
    stop("Unsupported `parent` type", call. = FALSE)
  }

  # Forward parent constructor arguments via `...` rather than copying
  # parent's formals. This ensures they're evaluated in the correct
  # environment and we can override parent properties. The downside is
  # that we lose an explicit list of all properties.
  if (length(formals(parent_fun)) > 0L) {
    args <- c(alist(... = ), arg_info$self)
    parent_call <- new_call(parent_name, list(quote(...)))
  } else {
    args <- arg_info$self
    parent_call <- new_call(parent_name, list())
  }
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

  self_arg_nms <- names2(properties)

  self_args <- as.pairlist(lapply(
    setNames(, self_arg_nms),
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
