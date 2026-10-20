#' Native API
#'
#' S7 provides a native API for retrieving an S7 class definition and reading
#' properties without depending on how S7 stores them.
#'
#' @section C and C++:
#' Add S7 to both `Imports` and `LinkingTo` in your package's `DESCRIPTION`, and
#' import an S7 function in `NAMESPACE` (for example, `importFrom(S7, S7_class)`).
#' This loads S7 before your package's compiled code uses the API.
#'
#' Include `<S7.c>` in exactly one C or C++ source file in your package. Call
#' `S7_init_api()` from your package's `R_init_<pkg>()` routine to initialize the
#' function pointers before using them. For example, in `src/init.c`:
#'
#' ```c
#' #include <S7.c>
#'
#' void R_init_mypkg(DllInfo *dll) {
#'   S7_init_api();
#'   /* Keep your package's routine registration here. */
#' }
#' ```
#'
#' If the initialization routine is defined in C++, give it C linkage with
#' `extern "C"`. Other source files include `<S7.h>`, which declares the shared
#' function pointers:
#'
#' ```c
#' extern SEXP (*S7_get_class)(SEXP object);
#' extern SEXP (*S7_get_prop)(SEXP object, SEXP name);
#' ```
#'
#' Initialization happens once when your package's DLL is loaded. Subsequent
#' calls use these pointers directly, without an initialization check.
#'
#' `S7_get_class()` returns the S7 class definition stored on `object`, or
#' `R_NilValue` if it has none. It resolves shared class references and supports
#' objects created with older versions of S7. Unlike the R function [S7_class()],
#' it does not construct class specifications for base, S3, or S4 objects that
#' have no S7 class definition.
#'
#' `S7_get_prop()` returns the value of a property, with the same getter and
#' storage behavior as [prop()]. Callers must supply `name` as a character vector
#' of length one containing a non-missing string. The native API assumes this
#' precondition is met; behavior for other inputs is unsupported.
#' Partial matching is not performed. Non-S7 objects and unknown properties
#' signal R errors. Errors from getters propagate to the caller.
#'
#' Both functions use R's usual memory protection rules. Keep arguments protected
#' during the call, and protect a returned value before making further allocating
#' R API calls. Returned values may be shared with the object; duplicate them
#' before modifying them. Call these functions only on R's main thread.
#'
#' For example, a `.Call` routine can read a property as follows:
#'
#' ```c
#' #include <S7.h>
#'
#' SEXP object_value(SEXP object) {
#'   SEXP name = PROTECT(Rf_mkString("value"));
#'   SEXP value = PROTECT(S7_get_prop(object, name));
#'   UNPROTECT(2);
#'   return value;
#' }
#' ```
#'
#' @section Other native bindings:
#' The functions are registered with `R_RegisterCCallable()`. Bindings that do
#' not use the header, such as Rust bindings, can retrieve them with
#' `R_GetCCallable("S7", "S7_get_class")` and
#' `R_GetCCallable("S7", "S7_get_prop")`, using the function pointer types above
#' and the C calling convention. S7's namespace must be loaded before looking
#' them up. These bindings cache the pointers themselves and do not need
#' `S7_init_api()`.
#' These entry points can allocate and signal R errors, including errors from
#' R code called by a getter. Use the binding's normal R error-handling mechanism.
#'
#' The registered names and signatures are public API. Future S7 releases will
#' preserve them; an incompatible signature would use a new registered name.
#' Other native routines in S7 are internal.
#'
#' @name native-api
#' @aliases S7_init_api S7_get_class S7_get_prop
NULL
