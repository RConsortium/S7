#' Native API
#'
#' S7 provides a stable native API for retrieving class definitions and reading
#' properties without depending on how S7 stores them.
#'
#' * `SEXP S7_get_class(SEXP object)` returns the stored S7 class definition, or
#'   `R_NilValue` if there is none. Unlike [S7_class()], it does not construct
#'   class specifications for base, S3, or S4 objects.
#' * `SEXP S7_get_prop(SEXP object, SEXP name)` reads a property, including custom
#'   getters, as [prop()] does. `name` must be a character vector of length one
#'   containing a non-missing string.
#'
#' @section C and C++:
#' Add S7 to `Imports` and `LinkingTo` in `DESCRIPTION`. In `NAMESPACE`, add
#' `importFrom(S7, S7_class)` and `useDynLib(mypkg, .registration = TRUE)`.
#'
#' This example `src/init.c` initializes the API and registers a `.Call` routine
#' that reads the `value` property:
#'
#' ```c
#' #include <S7-init.h>
#'
#' static SEXP object_value(SEXP object) {
#'   SEXP name = PROTECT(Rf_mkString("value"));
#'   SEXP value = S7_get_prop(object, name);
#'   UNPROTECT(1);
#'   return value;
#' }
#'
#' static const R_CallMethodDef CallEntries[] = {
#'   {"object_value", (DL_FUNC) &object_value, 1},
#'   {NULL, NULL, 0}
#' };
#'
#' void R_init_mypkg(DllInfo *dll) {
#'   S7_init_api();
#'   R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
#'   R_useDynamicSymbols(dll, FALSE);
#'   R_forceSymbols(dll, TRUE);
#' }
#' ```
#'
#' An R wrapper in the package can call `.Call(object_value, object)`.
#' Other C/C++ source files include `<S7.h>`. If the initialization routine is
#' written in C++, declare it `extern "C"`.
#'
#' @section Other native bindings:
#' Rust and other bindings can cache pointers from
#' `R_GetCCallable("S7", "S7_get_class")` and
#' `R_GetCCallable("S7", "S7_get_prop")` after loading S7, using the signatures
#' above and the C calling convention.
#'
#' @name native-api
#' @aliases S7_init_api S7_get_class S7_get_prop
NULL
