new_constructor <- function(parent, properties) {
  properties <- as_properties(properties)
  arg_info <- constructor_args(parent, properties)
  self_args <- as_names(names(arg_info$self), named = TRUE)

  if (identical(parent, S7_object) || (is_class(parent) && parent@abstract)) {
    return(new_function(
      args = arg_info$self,
      body = as.call(c(quote(`{`),
        # Force all promises here so that any errors are signaled from
        # the constructor() call instead of the new_object() call.
        unname(self_args),
        new_call("new_object", c(list(quote(S7_object())), self_args))
      )),
      env = asNamespace("S7")
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
    stop("Unsupported `parent` type", call. = FALSE)
  }

  # ensure default value for `...` is empty
  if ("..." %in% names(args)) {
    args[names(args) == "..."] <- list(quote(expr = ))
  }

  parent_args <- as_names(names(arg_info$parent), named = TRUE)
  names(parent_args)[names(parent_args) == "..."] <- ""
  parent_call <- new_call(parent_name, parent_args)
  body <- new_call("new_object", c(parent_call, self_args))

  env <- new.env(parent = asNamespace("S7"))
  env[[parent_name]] <- parent_fun

  new_function(args, body, env)
}

constructor_args <- function(parent, properties = list()) {
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
    function(name) prop_default(properties[[name]]))
  )

  list(parent = parent_args,
       self = self_args)
}


# helpers -----------------------------------------------------------------

is_property_dynamic <- function(x) is.function(x$getter)

missing_args <- function(names) {
  lapply(setNames(, names), function(i) quote(class_missing))
}
new_call <- function(call, args) {
  as.call(c(list(as.name(call)), args))
}

as_names <- function(x, named = FALSE) {
  if (named) {
    names(x) <- x
  }
  lapply(x, as.name)
}
