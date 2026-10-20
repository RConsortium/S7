#define R_NO_REMAP
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <S7.c>

static SEXP native_class(SEXP object) {
  return S7_get_class(object);
}

static SEXP native_prop(SEXP object, SEXP name) {
  return S7_get_prop(object, name);
}

static SEXP registered_class(SEXP object) {
  SEXP (*fn)(SEXP) =
    (SEXP (*)(SEXP)) R_GetCCallable("S7", "S7_get_class");
  return fn(object);
}

static SEXP registered_prop(SEXP object, SEXP name) {
  SEXP (*fn)(SEXP, SEXP) =
    (SEXP (*)(SEXP, SEXP)) R_GetCCallable("S7", "S7_get_prop");
  return fn(object, name);
}

extern SEXP cpp_class(SEXP);
extern SEXP cpp_prop(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"native_class", (DL_FUNC) &native_class, 1},
  {"native_prop", (DL_FUNC) &native_prop, 2},
  {"registered_class", (DL_FUNC) &registered_class, 1},
  {"registered_prop", (DL_FUNC) &registered_prop, 2},
  {"cpp_class", (DL_FUNC) &cpp_class, 1},
  {"cpp_prop", (DL_FUNC) &cpp_prop, 2},
  {NULL, NULL, 0}
};

void R_init_nativeapi(DllInfo *dll) {
  S7_init_api();
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
