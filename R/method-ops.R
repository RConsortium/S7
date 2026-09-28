base_ops <- NULL
base_matrix_ops <- NULL
ops_no_method <- new.env(parent = emptyenv())

on_load_define_ops <- function() {
  # Operator generics belong to base and accept only their operands
  env <- asNamespace("base")
  base_ops <<- lapply(
    setNames(, group_generics()$Ops),
    new_generic,
    dispatch_args = c("e1", "e2"),
    fun = new_function(alist(e1 = , e2 = ), quote(S7::S7_dispatch()), env)
  )
  # R dispatches `!` through the `Ops` group, but it's always unary
  base_ops[["!"]] <<- new_generic(
    "!",
    dispatch_args = "e1",
    fun = new_function(alist(e1 = ), quote(S7::S7_dispatch()), env)
  )

  base_matrix_ops <<- lapply(
    setNames(, group_generics()$matrixOps),
    new_generic,
    dispatch_args = c("x", "y"),
    fun = new_function(alist(x = , y = ), quote(S7::S7_dispatch()), env)
  )
}

#' @export
Ops.S7_object <- function(e1, e2) {
  out <-
    .External2(method_call_, base_ops[[.Generic]], environment(), ops_no_method)
  if (identical(out, ops_no_method)) {
    if (!missing(e2) && S7_inherits(e1) && S7_inherits(e2)) {
      method_lookup_error(.Generic, list(e1 = e1, e2 = e2))
    }
    # Must call NextMethod() directly in the method, not wrapped in an
    # anonymous function.
    NextMethod()
  } else {
    # R makes operator results visible, even when the method returns invisibly
    out
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
