#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP method_(SEXP, SEXP, SEXP, SEXP);
extern SEXP method_call_(SEXP, SEXP, SEXP);
extern SEXP S7_class_(SEXP, SEXP);
extern SEXP S7_object_(void);
extern SEXP prop_(SEXP, SEXP);
extern SEXP prop_set_(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"method_", (DL_FUNC) &method_, 4},
    {"method_call_", (DL_FUNC) &method_call_, 3},
    {"S7_object_", (DL_FUNC) &S7_object_, 0},
    {"prop_", (DL_FUNC) &prop_, 2},
    {"prop_set_", (DL_FUNC) &prop_set_, 4},
    {NULL, NULL, 0}
};

SEXP sym_ANY;
SEXP sym_S7_class;

SEXP sym_name;
SEXP sym_parent;
SEXP sym_package;
SEXP sym_properties;
SEXP sym_abstract;
SEXP sym_constructor;
SEXP sym_validator;
SEXP sym_getter;

SEXP sym_dot_should_validate;
SEXP sym_dot_getting_prop;
SEXP sym_dot_setting_prop;

SEXP ns_S7;


void R_init_S7(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    sym_ANY = Rf_install("ANY");
    sym_S7_class = Rf_install("S7_class");

    sym_name = Rf_install("name");
    sym_parent = Rf_install("parent");
    sym_package = Rf_install("package");
    sym_properties = Rf_install("properties");
    sym_abstract = Rf_install("abstract");
    sym_constructor = Rf_install("constructor");
    sym_validator = Rf_install("validator");
    sym_getter = Rf_install("getter");
    sym_dot_should_validate = Rf_install(".should_validate");
    sym_dot_getting_prop = Rf_install(".getting_prop");
    sym_dot_setting_prop = Rf_install(".setting_prop");

    ns_S7 = Rf_findVarInFrame(R_NamespaceRegistry, Rf_install("S7"));
}
