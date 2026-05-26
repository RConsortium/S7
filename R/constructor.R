new_constructor <- function(
  parent,
  properties,
  envir = asNamespace("S7"),
  package = NULL
) {
  properties <- as_properties(properties)
  arg_info <- constructor_args(parent, properties, envir, package)
  self_args <- as_names(names(arg_info$self), named = TRUE)

  if (
    identical(parent, S7_object) ||
      is_S4_class(parent) ||
      (is_class(parent) && parent@abstract)
  ) {
    if (is_S4_class(parent)) {
      parent_expr <- quote(.S4_parent_object(.S4_parent))
      env <- new.env(parent = envir)
      env$.S4_parent <- parent
      env$.S4_parent_object <- S4_parent_object
    } else {
      parent_expr <- if (has_S7_symbols(envir, "S7_object")) {
        quote(S7_object())
      } else {
        quote(S7::S7_object())
      }
      env <- envir
    }
    new_object_call <- new_call(
      if (has_S7_symbols(envir, "new_object")) {
        "new_object"
      } else {
        c("S7", "new_object")
      },
      c(list(parent_expr), self_args)
    )

    return(new_function(
      args = arg_info$self,
      body = as.call(c(
        quote(`{`),
        # Force all promises here so that any errors are signaled from
        # the constructor() call instead of the new_object() call.
        unname(self_args),
        new_object_call
      )),
      env = env
    ))
  }

  if (is_class(parent)) {
    parent_name <- parent@name
    parent_fun <- parent
    args <- modify_list(arg_info$parent, arg_info$self)
  } else if (is_base_class(parent)) {
    parent_name <- parent$constructor_name
    parent_fun <- parent$constructor
    args <- modify_list(arg_info$parent, arg_info$self)
  } else if (is_S3_class(parent)) {
    parent_name <- paste0("new_", parent$class[[1]])
    parent_fun <- parent$constructor
    args <- formals(parent$constructor)
    args[names(arg_info$self)] <- arg_info$self
  } else {
    # user facing error in S7_class()
    stop2("Unsupported `parent` type.", call = NULL)
  }

  # ensure default value for `...` is empty
  if ("..." %in% names(args)) {
    args[names(args) == "..."] <- list(quote(expr = ))
  }

  parent_args <- as_names(names(arg_info$parent), named = TRUE)
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

  self_arg_nms <- names2(properties)

  if (is_class(parent) && !parent@abstract) {
    # Remove any parent properties; can't use parent_args() since the constructor
    # might automatically set some properties.
    self_arg_nms <- setdiff(self_arg_nms, names2(parent@properties))
  }

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

S4_parent_object <- function(parent) {
  prototype <- parent@prototype
  if (".Data" %in% names(parent@slots)) {
    methods::slot(prototype, ".Data")
  } else {
    S7_object()
  }
}
