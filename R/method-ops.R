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

# Bridge base operators to S7 operators for S3/S4 registration
register_ops_bridge <- function(generic, signatures, env) {
  group <- ops_group(generic@name)
  if (is.null(group)) {
    return(invisible())
  }

  classes <- unique(unlist(lapply(signatures, function(sig) {
    lapply(sig, ops_bridge_class)
  })))

  for (class in classes) {
    # Don't clobber an existing group methods
    if (has_s3_method(group, class, env)) {
      next
    }

    # Re-use `Ops.S7_object`/`matrixOps.S7_object` to avoid conflicting methods
    generic <- if (group == "Ops") Ops.S7_object else matrixOps.S7_object
    registerS3method(group, class, generic, envir = env)
  }
  invisible()
}

has_s3_method <- function(generic, class, env) {
  !is.null(utils::getS3method(generic, class, envir = env, optional = TRUE))
}

ops_bridge_class <- function(x) {
  switch(
    class_type(x),
    S7_S3 = x$class[[1]],
    S4 = x@className,
    NULL
  )
}
