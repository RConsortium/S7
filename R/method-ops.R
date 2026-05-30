base_ops <- NULL
base_matrix_ops <- NULL

on_load_define_ops <- function() {
  base_ops <<- lapply(
    setNames(, group_generics()$Ops),
    new_generic,
    dispatch_args = c("e1", "e2")
  )
  base_matrix_ops <<- lapply(
    setNames(, group_generics()$matrixOps),
    new_generic,
    dispatch_args = c("x", "y")
  )
}

#' @export
Ops.S7_object <- function(e1, e2) {
  cnd <- tryCatch(
    return(base_ops[[.Generic]](e1, e2)),
    S7_error_method_not_found = function(cnd) cnd
  )

  if (S7_inherits(e1) && S7_inherits(e2)) {
    stop(cnd)
  } else {
    # Must call NextMethod() directly in the method, not wrapped in an
    # anonymous function.
    NextMethod()
  }
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod, S7_object)
#' @exportS3Method NULL
chooseOpsMethod.S7_object <- function(x, y, mx, my, cl, reverse) TRUE

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(matrixOps, S7_object)
#' @exportS3Method NULL
matrixOps.S7_object <- function(x, y) {
  base_matrix_ops[[.Generic]](x, y)
}

#' @export
Ops.S7_super <- Ops.S7_object

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod, S7_super)
chooseOpsMethod.S7_super <- chooseOpsMethod.S7_object

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(matrixOps, S7_super)
matrixOps.S7_super <- matrixOps.S7_object

# S3 bridges for base operators -------------------------------------------

# Base R's group-generic dispatch only calls `Ops.S7_object` when one operand
# is an S7 object. So a method like `method(`+`, list(new_S3_class("foo"),
# class_any))` is invisible to `foo + 10`, because `foo` is an informal S3
# object, not an S7 object (#544). To bridge this gap, whenever an S7 method is
# registered for a base operator with an S3 class in its signature, we also
# register a base-visible S3 method (e.g. `+.foo`) that re-enters S7 dispatch.
register_ops_bridges <- function(generic, signature, env) {
  group <- ops_generic_group(generic)
  if (is.null(group)) {
    return(invisible())
  }

  for (class in ops_s3_classes(signature)) {
    register_ops_bridge(generic@name, group, class, env)
  }
  invisible()
}

# `NULL` unless `generic` is one of S7's base operator generics. We check
# identity (not just the name) so a user generic that happens to be named `+`
# isn't mistaken for the base operator.
ops_generic_group <- function(generic) {
  if (!is_S7_generic(generic)) {
    return(NULL)
  }
  name <- generic@name
  if (identical(generic, base_ops[[name]])) {
    "Ops"
  } else if (identical(generic, base_matrix_ops[[name]])) {
    "matrixOps"
  } else {
    NULL
  }
}

ops_s3_classes <- function(signature) {
  classes <- character()
  for (sig in flatten_signature(signature)) {
    for (class in sig) {
      if (identical(class_type(class), "S7_S3")) {
        classes <- c(classes, class$class[[1]])
      }
    }
  }
  unique(classes)
}

register_ops_bridge <- function(op, group, class, env) {
  existing <- getS3method(op, class, optional = TRUE, envir = env)
  if (isTRUE(attr(existing, "S7_ops_bridge"))) {
    # Already bridged; the original method is preserved in the existing bridge.
    return(invisible())
  }

  # Preserve any pre-existing S3 behaviour (a specific `op.class` method, or
  # failing that the group `Ops.class`/`matrixOps.class` method) so the bridge
  # can fall back to it for signatures the user hasn't given an S7 method.
  original <- existing %||%
    getS3method(group, class, optional = TRUE, envir = env)
  registerS3method(op, class, new_ops_bridge(original), envir = env)
  invisible()
}

new_ops_bridge <- function(original = NULL) {
  force(original)
  bridge <- function(e1, e2) {
    generic <- base_ops[[.Generic]] %||% base_matrix_ops[[.Generic]]
    cnd <- tryCatch(
      return(generic(e1, e2)),
      S7_error_method_not_found = function(cnd) cnd
    )

    if (S7_inherits(e1) && S7_inherits(e2)) {
      stop(cnd)
    } else if (!is.null(original)) {
      call_with_generic(original, e1, e2, .Generic)
    } else {
      NextMethod()
    }
  }
  attr(bridge, "S7_ops_bridge") <- TRUE
  bridge
}

# Group generic methods like `Ops.factor` rely on `.Generic`, but S7 dispatch
# invokes methods directly without setting it. Make `.Generic` lexically
# available before calling the preserved method.
call_with_generic <- function(method, e1, e2, generic) {
  env <- new.env(parent = environment(method) %||% baseenv())
  env$.Generic <- generic
  environment(method) <- env
  method(e1, e2)
}
