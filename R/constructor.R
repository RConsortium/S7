new_constructor <- function(parent, properties) {
  args <- constructor_args(parent, properties)

  self_args <- as_names(args$self, named = TRUE)

  if (is_class(parent)) {
    if (identical(parent, R7_object)) {
      parent_call <- NULL
      env <- asNamespace("R7")
    } else {
      parent_name <- parent@name
      parent_args <- as_names(args$parent, named = TRUE)
      parent_call <- as.call(c(list(as.name(parent_name)), parent_args))

      env <- new.env(parent = asNamespace("R7"))
      env[[parent_name]] <- parent
    }

    constructor_args <- lapply(setNames(, args$constructor), function(i) quote(expr = ))
  } else if (is_s3_class(parent)) {
    parent_args <- as_names(args$parent, named = TRUE)
    parent_call <- as.call(c(list(quote(s3_constructor)), parent_args))
    env <- new.env(parent = asNamespace("R7"))
    env$s3_constructor <- parent$constructor

    constructor_args <- formals(parent$constructor)
    constructor_args[args$constructor] <- lapply(setNames(, args$constructor), function(i) quote(expr = ))
  } else {
    # user facing error in R7_class()
    stop("Unsupported `parent` type", call. = FALSE)
  }
  call <- as.call(c(list(quote(new_object), parent_call), self_args))

  f <- function() {}
  formals(f) <- constructor_args
  body(f) <- call
  environment(f) <- env
  attr(f, "srcref") <- NULL

  f
}

constructor_args <- function(parent, properties = list()) {
  parent_args <- names2(formals(class_constructor(parent)))

  self_args <- names2(properties)
  if (is_class(parent)) {
    # Remove dynamic arguments
    self_args <- self_args[vlapply(properties, function(x) is.null(x$getter))]
    # Remove any parent properties; can't use parent_args() since the constructor
    # might automatically set some properties.
    self_args <- setdiff(self_args, names2(parent@properties))
  }

  constructor_args <- union(parent_args, self_args)

  list(
    parent = parent_args,
    self = self_args,
    constructor = constructor_args
  )
}
