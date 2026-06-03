#' Register an S7 method for a generic
#'
#' @description
#' A generic defines the interface of a function. Once you have created a
#' generic with [new_generic()], you provide implementations for specific
#' signatures by registering methods with `method<-`.
#'
#' The goal is for `method<-` to be the single function you need when working
#' with S7 generics or S7 classes. This means that as well as registering
#' methods for S7 classes on S7 generics, you can also register methods for
#' S7 classes on S3 or S4 generics, and S3 or S4 classes on S7 generics.
#' But this is not a general method registration function: at least one of
#' `generic` and `signature` needs to be from S7.
#'
#' Note that if you are writing a package, you must call [methods_register()]
#' in your `.onLoad`. This ensures that all methods are dynamically registered
#' when needed.
#'
#' @param generic A generic function, i.e. an [S7 generic][new_generic],
#'   an [external generic][new_external_generic], an [S3 generic][UseMethod],
#'   or an [S4 generic][methods::setGeneric].
#' @param signature A method signature.
#'
#'   For single-dispatch generics, this must be one of the
#'   following:
#'
#'   * An S7 class (created by [new_class()]).
#'   * An S7 union (created by [new_union()]).
#'   * An S3 class (created by [new_S3_class()]) or `NULL`.
#'   * An S4 class (created by [methods::getClass()] or [methods::new()]).
#'   * A base type like [class_logical], [class_integer], or [class_numeric].
#'   * A special type like [class_missing] or [class_any].
#'   * A length-1 list containing any of the above.
#'
#'   For generics that use multiple dispatch, this must be a list of any of
#'   the above types.
#' @param value A function that implements the generic specification for the
#'   given `signature`, or `NULL` to unregister an existing method.
#' @returns The `generic`, invisibly.
#' @export
#' @examples
#' # Create a generic
#' bizarro <- new_generic("bizarro", "x")
#' # Register some methods
#' method(bizarro, class_numeric) <- function(x) rev(x)
#' method(bizarro, new_S3_class("data.frame")) <- function(x) {
#'   x[] <- lapply(x, bizarro)
#'   rev(x)
#' }
#'
#' # Using a generic calls the methods automatically
#' bizarro(head(mtcars))
#'
#' # Unregister a method by assigning `NULL`
#' method(bizarro, class_numeric) <- NULL
`method<-` <- function(generic, signature, value) {
  if (is.null(value)) {
    unregister_method(generic, signature, env = parent.frame())
  } else {
    register_method(generic, signature, value, env = parent.frame())
  }
  invisible(generic)
}

register_method <- function(
  generic,
  signature,
  method,
  env = parent.frame(),
  package = packageName(env),
  call = sys.call(-1L)
) {
  generic <- as_generic(generic, call = call)
  signature <- as_signature(signature, generic, call = call)

  if (is_external_generic(generic) && isNamespaceLoaded(generic$package)) {
    generic <- as_generic(
      getFromNamespace(generic$name, generic$package),
      call = call
    )
  }

  # Register in current session
  signatures <- flatten_signature(signature)
  if (is_S7_generic(generic)) {
    for (sig in signatures) {
      register_S7_method(generic, sig, method, call = call)
    }
    register_ops_bridge(generic, signatures, env)
  } else if (is_S3_generic(generic)) {
    for (sig in signatures) {
      register_S3_method(generic, sig, method, env, call = call)
    }
  } else if (is_S4_generic(generic)) {
    for (sig in signatures) {
      register_S4_method(generic, sig, method, env, call = call)
    }
  }

  # if we're inside a package, we also need to be able register methods
  # when the package is loaded
  if (!is.null(package) && !is_local_generic(generic, package)) {
    generic <- as_external_generic(generic, env)
    external_methods_add(package, generic, signature, method)
  }

  invisible(generic)
}

unregister_method <- function(
  generic,
  signature,
  env = parent.frame(),
  package = packageName(env),
  call = sys.call(-1L)
) {
  generic <- as_generic(generic, call = call)
  signature <- as_signature(signature, generic, call = call)

  if (is_external_generic(generic) && isNamespaceLoaded(generic$package)) {
    generic <- as_generic(
      getFromNamespace(generic$name, generic$package),
      call = call
    )
  }

  # Unregister in current session
  if (is_S7_generic(generic)) {
    unregister_S7_method(generic, signature)
  } else if (is_S3_generic(generic)) {
    stop2("Can't unregister methods for S3 generics", call = call)
  } else if (is_S4_generic(generic)) {
    stop2("Can't unregister methods for S4 generics", call = call)
  }

  # If we're inside a package, also remove from the deferred external
  # methods table so the method isn't re-registered on package load.
  if (!is.null(package) && !is_local_generic(generic, package)) {
    generic <- as_external_generic(generic)
    external_methods_remove(package, generic, signature)
  }

  invisible(generic)
}

register_S7_method <- function(
  generic,
  signature,
  method,
  call = sys.call(-1L)
) {
  check_method(
    method,
    generic,
    name = method_name(generic, signature),
    call = call
  )
  method <- S7_method(method, generic = generic, signature = signature)
  generic_add_method(generic, signature, method)

  invisible()
}

unregister_S7_method <- function(generic, signature) {
  signatures <- flatten_signature(signature)
  for (signature in signatures) {
    generic_remove_method(generic, signature)
  }
  invisible()
}

flatten_signature <- function(signature) {
  # Unpack unions
  sig_is_union <- vlapply(signature, is_union)
  signature[sig_is_union] <- lapply(signature[sig_is_union], "[[", "classes")
  signature[!sig_is_union] <- lapply(signature[!sig_is_union], list)

  # Create grid of indices
  indx <- lapply(signature, seq_along)
  comb <- as.matrix(rev(do.call("expand.grid", rev(indx))))
  colnames(comb) <- NULL

  rows <- lapply(1:nrow(comb), function(i) comb[i, ])
  lapply(rows, function(row) Map("[[", signature, row))
}

as_signature <- function(signature, generic, call = sys.call(-1L)) {
  if (inherits(signature, "S7_signature")) {
    return(signature)
  }

  n <- generic_n_dispatch(generic)
  if (n == 1) {
    # Accept a bare list of length 1 too, for symmetry with multi-dispatch
    # generics where a list is required (#555).
    if (is.list(signature) && !is.object(signature) && length(signature) == 1) {
      signature <- signature[[1]]
    }
    new_signature(list(as_class(signature, arg = "signature")))
  } else {
    check_signature_list(signature, n, call = call)
    for (i in seq_along(signature)) {
      signature[i] <- list(as_class(
        signature[[i]],
        arg = sprintf("signature[[%i]]", i)
      ))
    }
    new_signature(signature)
  }
}

check_signature_list <- function(
  x,
  n,
  arg = "signature",
  call = sys.call(-1L)
) {
  if (!is.list(x) || is.object(x)) {
    stop2(
      sprintf("`%s` must be a list for multidispatch generics.", arg),
      call = call
    )
  }
  if (length(x) != n) {
    stop2(sprintf("`%s` must be length %i.", arg, n), call = call)
  }
}

new_signature <- function(x) {
  class(x) <- "S7_signature"
  x
}

check_method <- function(
  method,
  generic,
  name = paste0(generic@name, "(???)"),
  call = sys.call(-1L)
) {
  if (!is.function(method) || is.primitive(method)) {
    stop2(sprintf("%s must be a function.", name), call = call)
  }

  generic_formals <- formals(args(generic))
  method_formals <- formals(method)
  generic_args <- names(generic_formals)
  method_args <- names(method_formals)

  if (!"..." %in% generic_args && !identical(generic_formals, method_formals)) {
    msg <- sprintf(
      "%s() generic lacks `...` so method formals must match generic formals exactly.",
      generic@name
    )
    bullets <- c(
      sprintf(
        "- generic formals: %s",
        show_args(generic_formals, name = generic@name)
      ),
      sprintf(
        "- method formals:  %s",
        show_args(method_formals, name = generic@name)
      )
    )
    stop2(c(msg, bullets), call = call)
  }

  n_dispatch <- length(generic@dispatch_args)
  has_dispatch <- length(method_formals) >= n_dispatch &&
    identical(method_args[1:n_dispatch], generic@dispatch_args)
  if (!has_dispatch) {
    msg <- sprintf(
      "%s() dispatches on %s, but %s has arguments %s.",
      generic@name,
      arg_names(generic@dispatch_args),
      name,
      arg_names(method_args)
    )
    stop2(msg, call = call)
  }

  empty_dispatch <- vlapply(
    method_formals[generic@dispatch_args],
    identical,
    quote(expr = )
  )
  if (any(!empty_dispatch)) {
    msg <- sprintf(
      "In %s, dispatch arguments (%s) must not have default values.",
      name,
      arg_names(generic@dispatch_args)
    )
    stop2(msg, call = call)
  }

  extra_args <- setdiff(names(generic_formals), c(generic@dispatch_args, "..."))
  for (arg in extra_args) {
    if (!arg %in% method_args) {
      warning(
        sprintf("%s doesn't have argument `%s`", name, arg),
        call. = FALSE
      )
    } else if (!identical(generic_formals[[arg]], method_formals[[arg]])) {
      msg <- sprintf(
        paste0(
          "In %s, default value of `%s` is not the same as the generic\n",
          "- Generic: %s\n",
          "- Method:  %s"
        ),
        name,
        arg,
        deparse1(generic_formals[[arg]]),
        deparse1(method_formals[[arg]])
      )
      warning(msg, call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' @export
print.S7_method <- function(x, ...) {
  signature <- method_signature(x@generic, x@signature)
  cat("<S7_method> ", signature, "\n", sep = "")

  attributes(x) <- NULL
  print(x)
}

arg_names <- function(x) {
  paste0(encodeString(x, quote = "`"), collapse = ", ")
}

method_name <- function(generic, signature) {
  method_args <- paste0(vcapply(signature, class_desc), collapse = ", ")
  sprintf("%s(%s)", generic@name, method_args)
}
