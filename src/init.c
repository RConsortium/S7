#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP class_names_(SEXP);
extern SEXP construct_signature_(SEXP);
extern SEXP get_r7_method_(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"class_names_", (DL_FUNC) &class_names_, 1},
    {"construct_signature_", (DL_FUNC) &construct_signature_, 1},
    {"get_r7_method_", (DL_FUNC) &get_r7_method_, 3},
    {NULL, NULL, 0}
};

void R_init_R7(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

