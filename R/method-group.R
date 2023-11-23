group_generic_Math <- NULL
group_generic_Ops <- NULL
group_generic_Complex <- NULL
group_generic_Sumary <- NULL

on_load_define_group_generics <- function() {
  group_generic_Math <<- new_generic("Math", "x")
  group_generic_Ops <<- new_generic("Ops", c("e1", "e2"))
  group_generic_Complex <<- new_generic("Complex", "z")
  group_generic_Summary <<- new_generic("Summary", "x", function(x, ..., na.rm = FALSE) {
    S7_dispatch()
  })
}

#' @export
Math.S7_object <- function(x, ...) {
  group_generic_Math(x, ..., .Generic = .Generic)
}

#' @export
Complex.S7_object <- function(z) {
  group_generic_Complex(z, .Generic = .Generic)
}

#' @export
Summary.S7_object <- function(..., na.rm = FALSE, .Generic) {
  group_generic_Summary(..., na.rm = TRUE, .Generic = .Generic)
}
