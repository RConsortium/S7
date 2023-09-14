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
  base_ops[[.Generic]](e1, e2)
}

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(chooseOpsMethod, S7_object)
chooseOpsMethod.S7_object <- function(x, y, mx, my, cl, reverse) TRUE

#' @rawNamespace if (getRversion() >= "4.3.0") S3method(matrixOps, S7_object)
matrixOps.S7_object <- NULL

on_load_define_matrixOps <- function() {
  if (getRversion() >= "4.4.0") {
    matrixOps.S7_object <<- function(x, y) {
      base_matrix_ops[[.Generic]](x, y)
    }
  } else {
    matrixOps.S7_object <<- function(e1, e2) {
      base_matrix_ops[[.Generic]](e1, e2)
    }
  }
}
