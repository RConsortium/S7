#ifndef S7_NATIVE_API_H
#define S7_NATIVE_API_H

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifdef __cplusplus
extern "C" {
#endif

// See help("native-api", package = "S7") for the API contract.
static inline SEXP S7_get_class(SEXP object) {
  static SEXP (*fn)(SEXP) = NULL;
  if (fn == NULL)
    fn = (SEXP (*)(SEXP)) R_GetCCallable("S7", "S7_get_class");
  return fn(object);
}

static inline SEXP S7_get_prop(SEXP object, SEXP name) {
  static SEXP (*fn)(SEXP, SEXP) = NULL;
  if (fn == NULL)
    fn = (SEXP (*)(SEXP, SEXP)) R_GetCCallable("S7", "S7_get_prop");
  return fn(object, name);
}

#ifdef __cplusplus
}
#endif

#endif
