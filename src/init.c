#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP method_(SEXP, SEXP, SEXP, SEXP);
extern SEXP method_call_(SEXP, SEXP, SEXP);
extern SEXP S7_class_(SEXP, SEXP);
extern SEXP S7_object_(void);

static const R_CallMethodDef CallEntries[] = {
    {"method_", (DL_FUNC) &method_, 4},
    {"method_call_", (DL_FUNC) &method_call_, 3},
    {"S7_object_", (DL_FUNC) &S7_object_, 0},
    {NULL, NULL, 0}
};

SEXP sym_ANY;

void R_init_S7(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    sym_ANY = Rf_install("ANY");
}
