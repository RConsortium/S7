#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

extern SEXP sym_S7_class;

extern SEXP sym_name;
extern SEXP sym_parent;
extern SEXP sym_package;
extern SEXP sym_properties;
extern SEXP sym_abstract;
extern SEXP sym_constructor;
extern SEXP sym_validator;

extern SEXP ns_S7;

static inline
int name_idx(SEXP list, const char* name) {
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);

  if (TYPEOF(names) == STRSXP)
    for (int i = 0, n = Rf_length(names); i < n; i++)
      if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0)
        return i;
  return -1;
}

static inline
SEXP extract_name(SEXP list, const char* name) {
  int i = name_idx(list, name);
  return i == -1 ? R_NilValue : VECTOR_ELT(list, i);
}

static inline
Rboolean has_name(SEXP list, const char* name) {
  return (Rboolean) name_idx(list, name) != -1;
}

static inline
Rboolean inherits2(SEXP object, const char* name) {
  // like inherits in R, but iterates over the class STRSXP vector
  // in reverse, since S7_* is typically at the tail.
  SEXP klass = Rf_getAttrib(object, R_ClassSymbol);
  if (TYPEOF(klass) == STRSXP) {
    for (int i = Rf_length(klass)-1; i >= 0; i--) {
      if (strcmp(CHAR(STRING_ELT(klass, i)), name) == 0)
        return TRUE;
    }
  }
  return FALSE;
}

inline static
Rboolean is_s7_object(SEXP object) {
  return inherits2(object, "S7_object");
}

inline static
Rboolean is_s7_class(SEXP object) {
  return inherits2(object, "S7_class");
}

static
__attribute__ ((noreturn))
void signal_prop_error_unknown_(SEXP object, SEXP name) {
  static SEXP signal_prop_error_unknown = NULL;
  if (signal_prop_error_unknown == NULL)
    signal_prop_error_unknown =
      Rf_findVarInFrame(ns_S7, Rf_install("signal_prop_error_unknown"));

  Rf_eval(Rf_lang3(signal_prop_error_unknown, object, name), ns_S7);
  while(1);
}

SEXP prop_(SEXP object, SEXP name) {

  if (!is_s7_object(object))
    goto error;

  SEXP name_rchar = STRING_ELT(name, 0);
  const char* name_char = CHAR(name_rchar);
  SEXP name_sym = Rf_installTrChar(name_rchar);

  SEXP S7_class = Rf_getAttrib(object, sym_S7_class);
  SEXP properties = Rf_getAttrib(S7_class, sym_properties);
  SEXP value = Rf_getAttrib(object, name_sym);

  // if value was accessed as an attr, we still need to validate to make sure
  // the attr is actually a known class property
  if (value != R_NilValue)
    goto validate;

  // property not in attrs, try to get value using the getter()
  if (properties == R_NilValue) goto validate;

  SEXP property = extract_name(properties, name_char);
  if (property == R_NilValue) goto validate;

  SEXP getter = extract_name(property, "getter");
  if (getter == R_NilValue) goto validate;

  if (TYPEOF(getter) == CLOSXP)
    // we validated property is in properties list when accessing getter()
    return Rf_eval(Rf_lang2(getter, object), ns_S7);


  validate:

  if(has_name(properties, name_char))
    return value;

  if (S7_class == R_NilValue &&
      is_s7_class(object) && (
          name_sym == sym_name  ||
          name_sym == sym_parent  ||
          name_sym == sym_package  ||
          name_sym == sym_properties  ||
          name_sym == sym_abstract  ||
          name_sym == sym_constructor  ||
          name_sym == sym_validator
    ))
      return value;

  error:

  signal_prop_error_unknown_(object, name);
  return R_NilValue; // unreachable, for compiler
}
