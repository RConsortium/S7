group_generic_Math <- NULL
group_generic_Ops <- NULL
group_generic_Complex <- NULL
group_generic_Summary <- NULL

on_load_define_group_generics <- function() {
  group_generic_Math <<- new_generic(
    "Math",
    "x",
    function(x, ..., .Generic) {
      S7_dispatch()
    }
  )

  group_generic_Ops <<- new_generic(
    "Ops",
    c("e1", "e2"),
    function(e1, e2, ..., .Generic) {
      S7_dispatch()
    }
  )

  group_generic_Complex <<- new_generic(
    "Complex",
    "z",
    function(z, ..., .Generic) {
      S7_dispatch()
    }
  )

  group_generic_Summary <<- new_generic(
    "Summary",
    "x",
    function(x, ..., na.rm = FALSE, .Generic) {
      S7_dispatch()
    }
  )
}

#' @export
Math.S7_object <- function(x, ...) {
  generic_fun <- get(.Generic, mode = "function", envir = baseenv())
  tryCatch(
    return(group_generic_Math(x, ..., .Generic = generic_fun)),
    S7_error_method_not_found = function(cnd) NULL
  )

  NextMethod()
}
