new_constructor <- function(parent, properties) {
  properties <- as_properties(properties)
  arg_info <- constructor_args(parent, properties)
  self_args <- as_names(names(arg_info$self), named = TRUE)

  if (identical(parent, S7_object) || (is_class(parent) && parent@abstract)) {
    return(new_function(
      args = arg_info$self,
      body = new_call("new_object", c(list(quote(S7_object())), self_args)),
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
  parent_constructor_formals <- formals(class_constructor(parent))
  parent_args <- names2(parent_constructor_formals)

  self_args <- names2(properties)
  # Remove dynamic arguments
  self_args <- self_args[vlapply(properties, function(x) is.null(x$getter))]
  if (is_class(parent) && !parent@abstract) {
    # Remove any parent properties; can't use parent_args() since the constructor
    # might automatically set some properties.
    self_args <- setdiff(self_args, names2(parent@properties))
  }

  self_args <- lapply(setNames(nm = self_args),
                      function(name) prop_default(properties[[name]]))
  parent_args <- parent_constructor_formals %||%
    structure(list(), names = character())

  list(parent = as.list(parent_args),
       self = as.list(self_args))
}

is_property_dynamic <- function(prop) is.function(x$getter)

# helpers -----------------------------------------------------------------

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
