#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP class_names_(SEXP);
extern SEXP object_class_(SEXP);
extern SEXP method_(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"method_", (DL_FUNC) &method_, 3},
    {NULL, NULL, 0}
};

SEXP parent_sym;
SEXP name_sym;

void R_init_R7(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    parent_sym = Rf_install("parent");
    name_sym = Rf_install("name");
}
