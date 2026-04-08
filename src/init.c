#include "compat.h"
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP method_(SEXP, SEXP, SEXP, SEXP);
extern SEXP method_call_(SEXP, SEXP, SEXP, SEXP);
extern SEXP test_call_(SEXP, SEXP, SEXP, SEXP);
extern SEXP S7_class_(SEXP, SEXP);
extern SEXP S7_object_(void);
extern SEXP prop_(SEXP, SEXP);
extern SEXP prop_set_(SEXP, SEXP, SEXP, SEXP);

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(method_, 4),
    CALLDEF(S7_object_, 0),
    CALLDEF(prop_, 2),
    CALLDEF(prop_set_, 4),
    {NULL, NULL, 0}
};

static const R_ExternalMethodDef ExternalEntries[] = {
    CALLDEF(method_call_, 2),
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
SEXP sym_obj_dispatch;
SEXP sym_dispatch_args;
SEXP sym_methods;
SEXP sym_S7_dispatch;
SEXP sym_name;

SEXP fn_base_quote;
SEXP fn_base_missing;

SEXP ns_S7;

SEXP R_TRUE, R_FALSE;
SEXP s7_proto_object;

static SEXP make_s7_proto_object(void)
{
    SEXP obj = PROTECT(Rf_allocS4Object());
    SEXP asS3_call = PROTECT(Rf_lang4(
        Rf_install("asS3"), obj, /*flag =*/ R_TRUE, /*complete =*/ R_FALSE
    ));
    obj = PROTECT(Rf_eval(asS3_call, R_BaseEnv));
    Rf_classgets(obj, Rf_mkString("S7_object"));

    UNPROTECT(3);
    return obj;
}

void R_init_S7(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, ExternalEntries);
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
    sym_obj_dispatch = Rf_install("obj_dispatch");
    sym_dispatch_args = Rf_install("dispatch_args");
    sym_methods = Rf_install("methods");
    sym_S7_dispatch = Rf_install("S7_dispatch");
    sym_name = Rf_install("name");

    fn_base_quote = Rf_eval(Rf_install("quote"), R_BaseEnv);
    fn_base_missing = Rf_eval(Rf_install("missing"), R_BaseEnv);

    ns_S7 = R_FindNamespace(Rf_mkString("S7"));
    R_PreserveObject(R_TRUE = Rf_ScalarLogical(1));
    R_PreserveObject(R_FALSE = Rf_ScalarLogical(0));
    R_PreserveObject(s7_proto_object = make_s7_proto_object());
}
