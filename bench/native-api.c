#define R_NO_REMAP
#include <S7.c>

static SEXP native_class(SEXP object, SEXP iterations) {
  int n = Rf_asInteger(iterations);
  SEXP result = R_NilValue;
  for (int i = 0; i < n; ++i)
    result = S7_get_class(object);
  return result;
}

static SEXP native_prop(SEXP object, SEXP name, SEXP iterations) {
  int n = Rf_asInteger(iterations);
  SEXP result = R_NilValue;
  for (int i = 0; i < n; ++i)
    result = S7_get_prop(object, name);
  return result;
}

static SEXP eval_call(SEXP call, SEXP env, SEXP iterations) {
  int n = Rf_asInteger(iterations);
  SEXP result = R_NilValue;
  for (int i = 0; i < n; ++i)
    result = Rf_eval(call, env);
  return result;
}

static const R_CallMethodDef CallEntries[] = {
  {"native_class", (DL_FUNC) &native_class, 2},
  {"native_prop", (DL_FUNC) &native_prop, 3},
  {"eval_call", (DL_FUNC) &eval_call, 3},
  {NULL, NULL, 0}
};

void R_init_s7nativebench(DllInfo *dll) {
  S7_init_api();
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
