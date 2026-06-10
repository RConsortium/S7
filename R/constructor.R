new_constructor <- function(
  parent,
  properties,
  envir = asNamespace("S7"),
  package = NULL
) {
  properties <- as_properties(properties)

  if (is_external_class(parent)) {
    return(new_external_constructor(parent, properties, envir, package))
  }

  arg_info <- constructor_args(parent, properties, envir, package)
  self_args <- as_names(names(arg_info$self), named = TRUE)

  if (identical(parent, S7_object) || (is_class(parent) && parent@abstract)) {
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

# Constructor for a class that inherits from an external class. The parent's
# package might not be loaded when the class is defined, so rather than inlining
# the parent's constructor arguments (which would require resolving the external
# class now), the constructor takes the child's own properties plus a `...` that
# is forwarded to the parent. The external class is only resolved the first time
# an object is constructed (when its package must be loaded anyway): at that
# point we rebuild the class with the resolved parent so that inherited
# properties, validation, and dispatch all behave as if the parent had been
# known up front. The completed class is cached for subsequent calls.
new_external_constructor <- function(parent, properties, envir, package) {
  properties <- properties[!vlapply(properties, prop_is_read_only)]
  self_args <- as.pairlist(lapply(
    setNames(, names2(properties)),
    function(name) prop_default(properties[[name]], envir, package)
  ))
  self_names <- as_names(names(self_args), named = TRUE)

  # Bind the resolver into the constructor's environment: the wrapper runs in
  # the consumer's namespace, which only imports S7's *exported* functions, so
  # this internal helper must be reachable lexically.
  env <- new.env(parent = envir)
  env$parent <- parent
  env$completed <- NULL
  env$complete_external_class <- complete_external_class

  body <- bquote(
    {
      if (is.null(completed)) {
        completed <<- complete_external_class(sys.function(), parent)
      }
      completed(..(self_names), ...)
    },
    splice = TRUE
  )

  new_function(c(self_args, alist(... = )), body, env)
}

# Rebuild a class that inherits from an external `parent`, now that the parent's
# package is loaded and the external class can be resolved.
complete_external_class <- function(child, parent) {
  new_class(
    child@name,
    parent = resolve_external_class_req(parent),
    package = child@package,
    properties = child@properties,
    abstract = child@abstract,
    validator = child@validator
  )
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
