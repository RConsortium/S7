#' @export
t4_class <- S7::new_class("t4_class", package = "t4")
S7::S4_register(t4_class)

# Register an S7 method for an S3 generic defined in another package (t3).
S7::method(t3_s3, t4_class) <- function(x) "s3-dispatch"

# Register an S7 method for an S4 generic defined in another package (t3).
S7::S4_register(t4_class)
S7::method(t3_s4, t4_class) <- function(x) "s4-dispatch"

# Register an S7 method for a base generic (defined in base R).
S7::method(`$`, t4_class) <- function(x, name) "base-dispatch"

.onLoad <- function(libname, pkgname) {
  S7::S7_on_load()
}

S7::S7_on_build()
