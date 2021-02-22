#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP class_names_(SEXP);
extern SEXP object_class_(SEXP);
extern SEXP method_(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"class_names_", (DL_FUNC) &class_names_, 1},
    {"object_class_", (DL_FUNC) &object_class_, 1},
    {"method_", (DL_FUNC) &method_, 2},
    {NULL, NULL, 0}
};

SEXP r7_methods_sym;
SEXP parent_sym;
SEXP name_sym;
SEXP object_class_sym;

void R_init_R7(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    r7_methods_sym = Rf_install(".r7_methods");
    parent_sym = Rf_install("parent");
    name_sym = Rf_install("name");
    object_class_sym = Rf_install("object_class");
}

