#' S7 Group Generics
#'
#' Group generics allow you to implement methods for many generics at once.
#' You cannot call a group generic directly; instead it is called automatically
#' by members of the group if a more specific method is not found. For example,
#' if you define a method for the `S7_Math` group generic, it will be called
#' when you call `abs()`, `sign()`, `sqrt()`, and many other similar generics
#' (see below for a complete list).
#'
#' @param x,z,e1,e2 Objects used for dispatch.
#' @param ...,na.rm Additional arguments passed to methods.
#' @param .Generic The name of the generic being dispatched on, i.e. if you've
#'   defined a method for `S7_Math` and the user calls `abs()` then `.Generic`
#'   will be `"abs"`.
#'
#'   Use `find_base_generic()` to find the base generic that corresponds to the
#'   generic name.
#' @details
#' # Methods
#'
#' The group generics contain the following methods:
#'
#' * `Ops`: `r group_generics_md("Ops")`
#' * `Math`: `r group_generics_md("Math")`
#' * `Summary`: `r group_generics_md("Summary")`
#' * `Complex`: `r group_generics_md("Complex")`
#' * `matrixOps`: `r group_generics_md("matrixOps")`
#'
#' @name S7_group_generics
NULL

#' @export
#' @rdname S7_group_generics
S7_Math <- NULL

#' @export
#' @rdname S7_group_generics
S7_Ops <- NULL

#' @export
#' @rdname S7_group_generics
S7_Complex <- NULL

#' @export
#' @rdname S7_group_generics
S7_Summary <- NULL

on_load_define_group_generics <- function() {
  S7_Math <<- new_generic("Math", "x", function(x, ..., .Generic) {
    S7_dispatch()
  })

  S7_Ops <<- new_generic("Ops", c("e1", "e2"), function(e1, e2, ..., .Generic) {
    S7_dispatch()
  })

  S7_Complex <<- new_generic("Complex", "z", function(z, ..., .Generic) {
    S7_dispatch()
  })

  S7_Summary <<- new_generic("Summary", "x", function(x, ..., na.rm = FALSE, .Generic) {
    S7_dispatch()
  })
}

#' @export
Math.S7_object <- function(x, ...) {
  tryCatch(
    return(S7_Math(x, ..., .Generic = .Generic)),
    S7_error_method_not_found = function(cnd) NULL
  )

  NextMethod()
}

#' @export
#' @rdname S7_group_generics
find_base_generic <- function(.Generic) {
  get(.Generic, mode = "function", envir = baseenv())
}


group_generics_md <- function(name) {
  paste0("`", group_generics()[[name]], "`", collapse = ", ")
}
