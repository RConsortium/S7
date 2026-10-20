#include "compat.h"

extern SEXP S7_class_(SEXP);
extern SEXP prop_(SEXP, SEXP);

SEXP S7_get_class(SEXP object) {
  return S7_class_(object);
}

SEXP S7_get_prop(SEXP object, SEXP name) {
  if (TYPEOF(name) != STRSXP || Rf_xlength(name) != 1 ||
      STRING_ELT(name, 0) == NA_STRING)
    Rf_error("`name` must be a single, non-missing string.");

  return prop_(object, name);
}
