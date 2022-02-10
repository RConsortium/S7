#' Register a R7 method for a generic
#'
#' @description
#' A generic defines the interface of a function. Once you have created a
#' generic with [new_generic()], you provide implementations for specific
#' signatures by registering methods with `method<-`
#'
#' The goal is for `method<-` to be the single function you need when working
#' with R7 generics or R7 classes. This means that as well as registering
#' methods for R7 classes on R7 generics, you can also register methods for
#' R7 classes on S3 or S4 generics, and S3 or S4 classes for R7 generics.
#' But this is not a general method registration function: at least one of
#' `generic` and `signature` needs to be from R7.
#'
#' @param generic A generic function, either created by [new_generic()],
#'   [new_external_generic()], or an existing S3 generic.
#' @param signature A method signature. For R7 generics that use single
#'   dispatch, this should be one of the following:
#'   * An R7 class (created by [new_class()]).
#'   * An R7 union (created by [new_union()]).
#'   * An S3 class (created by [s3_class()]).
#'   * An S4 class (created by [methods::getClass()] or [methods::new()]).
#'   * A base type specified either with its constructor (`logical`, `integer`,
#'     `double` etc) or its name (`"logical"`, `"integer"`, "`double`" etc).
#'   * A base union type specified by its name: `"numeric"`, `"atomic"`, or
#'     `"vector"`.
#'
#'   For R7 generics that use multiple dispatch, this can be a list of any of
#'   the above types.
#'
#'   For S3 generics, this must be an R7 class.
#' @param value A function that implements the generic specification for the
#'   given `signature`.
#' @export
#' @examples
#' # Create a generic
#' bizarro <- new_generic("bizarro", dispatch_args = "x")
#' # Register some methods
#' method(bizarro, "numeric") <- function(x) rev(x)
#' method(bizarro, s3_class("data.frame")) <- function(x) {
#'   x[] <- lapply(x, bizarro)
#'   rev(x)
#' }
#'
#' # Using a generic calls the methods automatically
#' bizarro(head(mtcars))
`method<-` <- function(generic, signature, value) {
  register_method(generic, signature, value)
  invisible(generic)
}

register_method <- function(generic, signature, method, package = packageName(parent.frame())) {
  signature <- as_signature(signature)
  generic <- as_generic(generic)

  if (is_external_generic(generic)) {
    if (!is.null(package)) {
      # method registration within package, so add to lazy registry
      external_methods_add(package, generic, signature, method)
    } else {
      # otherwise find the generic and register
      generic <- getFromNamespace(generic$name, asNamespace(generic$package))
      register_method(generic, signature, method)
    }
  } else if (is_s3_generic(generic)) {
    generic_name <- attr(generic, "name")

    if (length(signature) != 1 || class_type(signature[[1]]) != "r7") {
      msg <- sprintf(
        "When registering methods for S3 generic %s(), signature be a single R7 class",
        generic_name
      )
      stop(msg, call. = FALSE)
    }
    class <- signature[[1]]@name
    registerS3method(generic_name, class, method, envir = parent.frame())
  } else {
    check_method(method, signature, generic)
    register_r7_method(generic, signature, method)
  }

  invisible()
}

register_r7_method <- function(generic, signature, method) {
  # Flatten out unions to individual signatures
  signatures <- flatten_signature(signature)

  # Register each method
  for (signature in signatures) {
    method <- R7_method(method, generic = generic, signature = signature)
    generic_add_method(generic, signature, method)
  }

  invisible()
}

flatten_signature <- function(signature) {
  # Unpack unions
  sig_is_union <- vlapply(signature, is_union)
  signature[sig_is_union] <- lapply(signature[sig_is_union], prop, "classes")
  signature[!sig_is_union] <- lapply(signature[!sig_is_union], list)

  # Create grid of indices
  indx <- lapply(signature, seq_along)
  comb <- as.matrix(rev(do.call("expand.grid", rev(indx))))
  colnames(comb) <- NULL

  rows <- lapply(1:nrow(comb), function(i) comb[i, ])
  lapply(rows, function(row) Map("[[", signature, row))
}

as_generic <- function(x) {
  if (inherits(x, "R7_generic") || is_external_generic(x)) {
    return(x)
  }

  if (!is.function(x)) {
    msg <- sprintf("`generic` must be a function, not a %s", obj_desc(x))
    stop(msg, call. = FALSE)
  }

  # For now, assume that it's an S3 generic
  attr(x, "name") <- find_generic_name(x)
  class(x) <- "R7_S3_generic"
  x
}
is_s3_generic <- function(x) inherits(x, "R7_S3_generic")

find_generic_name <- function(generic) {
  env <- environment(generic) %||% baseenv()
  for (nme in names(env)) {
    if (identical(generic, env[[nme]])) {
      return(nme)
    }
  }

  stop("Can't find name of S3 `generic`", call. = FALSE)
}

as_signature <- function(signature) {
  if (!is.list(signature)) {
    signature <- list(signature)
  }

  for (i in seq_along(signature)) {
    signature[[i]] <- as_class(signature[[i]], arg = sprintf("signature[[%i]]", i))
  }
  signature
}

check_method <- function(method, signature, generic) {
  signature <- as_signature(signature)
  method_name <- method_name(generic, signature)

  if (!is.function(method)) {
    stop(sprintf("%s must be a function", method_name), call. = FALSE)
  }

  generic_formals <- formals(args(generic))
  method_formals <- formals(method)
  generic_args <- names(generic_formals)
  method_args <- names(method_formals)

  n_dispatch <- length(generic@dispatch_args)
  has_dispatch <- length(method_formals) >= n_dispatch &&
    identical(method_args[1:n_dispatch], generic@dispatch_args)
  if (!has_dispatch) {
    msg <- sprintf(
      "%s() dispatches on %s, but %s has arguments %s",
      generic@name,
      arg_names(generic@dispatch_args),
      method_name,
      arg_names(method_args)
    )
    stop(msg, call. = FALSE)
  }
  if ("..." %in% method_args && method_args[[n_dispatch + 1]] != "...") {
    msg <- sprintf(
      "In %s, `...` must come immediately after dispatch args (%s)",
      method_name,
      arg_names(generic@dispatch_args)
    )
    stop(msg, call. = FALSE)
  }
  empty_dispatch <- vlapply(method_formals[generic@dispatch_args], identical, quote(expr = ))
  if (any(!empty_dispatch)) {
    msg <- sprintf(
      "In %s, dispatch arguments (%s) must not have default values",
      method_name,
      arg_names(generic@dispatch_args)
    )
    stop(msg, call. = FALSE)
  }

  extra_args <- setdiff(names(generic_formals), c(generic@dispatch_args, "..."))
  for (arg in extra_args) {
    if (!arg %in% method_args) {
      warning(sprintf("%s doesn't have argument `%s`", method_name, arg), call. = FALSE)
    } else if (!identical(generic_formals[[arg]], method_formals[[arg]])) {
      msg <- sprintf(
        paste0(
          "In %s, default value of `%s` is not the same as the generic\n",
          "- Generic: %s\n",
          "- Method:  %s"
        ),
        method_name,
        arg,
        deparse1(generic_formals[[arg]]),
        deparse1(method_formals[[arg]])
      )
      warning(msg, call. = FALSE)
    }
  }

  invisible(TRUE)
}

# Class name when registering an R7 method
r7_class_name <- function(x) {
  switch(class_type(x),
    s3 = x,
    s4 = x@className,
    r7 = x@name,
    r7_base = x@name,
    stop("Unsupported")
  )
}

#' @export
print.R7_method <- function(x, ...) {
  signature <- method_signature(x@generic, x@signature)
  cat("<R7_method> ", signature, "\n", sep = "")

  attributes(x) <- NULL
  print(x)
}

arg_names <- function(x) {
  paste0(encodeString(x, quote = "`"), collapse = ", ")
}

method_name <- function(generic, signature) {
  method_args <- paste0(vcapply(signature, class_desc), collapse =", ")
  sprintf("%s(%s)", generic@name, method_args)
}
