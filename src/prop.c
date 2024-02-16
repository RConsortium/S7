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

extern SEXP sym_dot_should_validate;
extern SEXP sym_dot_setting_prop;

static inline
SEXP eval_here(SEXP lang) {
  PROTECT(lang);
  SEXP ans = Rf_eval(lang, ns_S7);
  UNPROTECT(1);
  return ans;
}

static __attribute__((noreturn))
  void signal_is_not_S7(SEXP object) {
    static SEXP check_is_S7 = NULL;
    if (check_is_S7 == NULL)
      check_is_S7 = Rf_findVarInFrame(ns_S7, Rf_install("check_is_S7"));

    // will signal error
    eval_here(Rf_lang2(check_is_S7, object));
    while(1);
  }


static __attribute__((noreturn))
  void signal_prop_error(const char* fmt, SEXP object, SEXP name) {
    static SEXP signal_prop_error = NULL;
    if (signal_prop_error == NULL)
      signal_prop_error = Rf_findVarInFrame(ns_S7, Rf_install("signal_prop_error"));

    eval_here(Rf_lang4(signal_prop_error, Rf_mkString(fmt), object, name));
    while(1);
  }

static __attribute__((noreturn))
  void signal_prop_error_unknown(SEXP object, SEXP name) {
    signal_prop_error("Can't find property %s@%s", object, name);
  }

static __attribute__((noreturn))
  void signal_error(SEXP errmsg) {
    PROTECT(errmsg);
    if(TYPEOF(errmsg) == STRSXP && Rf_length(errmsg) == 1)
      Rf_errorcall(R_NilValue, "%s", CHAR(STRING_ELT(errmsg, 0)));

    // fallback to calling base::stop(errmsg)
    static SEXP signal_error = NULL;
    if (signal_error == NULL)
      signal_error = Rf_findVarInFrame(ns_S7, Rf_install("signal_error"));

    eval_here(Rf_lang2(signal_error, errmsg));
    while(1);
  }

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
  return (Rboolean) (name_idx(list, name) != -1);
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

static inline
Rboolean is_s7_object(SEXP object) {
  return inherits2(object, "S7_object");
}

static inline
Rboolean is_s7_class(SEXP object) {
  return inherits2(object, "S7_class");
}

static inline
void check_is_S7(SEXP object) {
  if (is_s7_object(object))
    return;
  signal_is_not_S7(object);
}


SEXP prop_(SEXP object, SEXP name) {
  check_is_S7(object);

  SEXP name_rchar = STRING_ELT(name, 0);
  const char* name_char = CHAR(name_rchar);
  SEXP name_sym = Rf_installTrChar(name_rchar);

  SEXP S7_class = Rf_getAttrib(object, sym_S7_class);
  SEXP properties = Rf_getAttrib(S7_class, sym_properties);
  SEXP value = Rf_getAttrib(object, name_sym);

  // if value was accessed as an attr, we still need to validate to make sure
  // the attr is actually a known class property
  if (value == R_NilValue) {
    // property not in attrs, try to get value using the getter()
    SEXP property = extract_name(properties, name_char);
    SEXP getter = extract_name(property, "getter");
    if (TYPEOF(getter) == CLOSXP)
        // we validated property is in properties list when accessing getter()
        return eval_here(Rf_lang2(getter, object));
  }

  if (has_name(properties, name_char))
    return value;

  if (S7_class == R_NilValue &&
      is_s7_class(object) && (
          name_sym == sym_name  ||
          name_sym == sym_parent  ||
          name_sym == sym_package  ||
          name_sym == sym_properties  ||
          name_sym == sym_abstract  ||
          name_sym == sym_constructor  ||
          name_sym == sym_validator))
      return value;

  // Should the constructor always set default prop values on a object instance?
  // Maybe, instead, we can fallback here to checking for a default value from the
  // properties list.

  signal_prop_error_unknown(object, name);
  return R_NilValue; // unreachable, for compiler
}


static inline
Rboolean pairlist_contains(SEXP list, SEXP elem) {
  for (SEXP c = list; c != R_NilValue; c = CDR(c))
    if (CAR(c) == elem)
      return TRUE;
  return FALSE;
}

static inline
SEXP pairlist_remove(SEXP list, SEXP elem) {
  SEXP c0 = NULL, head = list;
  for (SEXP c = list; c != R_NilValue; c0 = c, c = CDR(c))
    if (CAR(c) == elem)
    {
      if (c0 == NULL)
        return CDR(c);
      else
      {
        SETCDR(c0, CDR(c));
        return head;
      }
    }

  Rf_error("Tried to remove non-existent element from pairlist");
  return R_NilValue;
}

static inline
Rboolean setter_callable_no_recurse(SEXP setter, SEXP object, SEXP name_sym,
                                    Rboolean* should_validate_obj) {

    SEXP no_recurse_list = Rf_getAttrib(object, sym_dot_setting_prop);
    if (TYPEOF(no_recurse_list) == LISTSXP) {
      // if there is a 'no_recurse' list, then this is not the top-most prop<-
      // call for this object, i.e, we're currently evaluating a `prop<-` call
      // called from within a custom property setter. We should only call
      // validate(object) once from the top-most prop<- call, after the last
      // custom setter() has returned.
      *should_validate_obj = FALSE;
      if (pairlist_contains(no_recurse_list, name_sym))
        return FALSE;
    }

    if (TYPEOF(setter) != CLOSXP)
      return FALSE; // setter not callable

    Rf_setAttrib(object, sym_dot_setting_prop,
                 Rf_cons(name_sym, no_recurse_list));
    return TRUE; // setter now marked non-recursive, safe to call

  // optimization opportunity: combine the actions of getAttrib()/setAttrib()
  // into one loop, so we can avoid iterating over ATTRIB(object) twice.
}

static inline
void setter_no_recurse_clear(SEXP object, SEXP name_sym) {
  SEXP list = Rf_getAttrib(object, sym_dot_setting_prop);
  list = pairlist_remove(list, name_sym);
  Rf_setAttrib(object, sym_dot_setting_prop, list);

  // optimization opportunity: same as setter_callable_no_recurse
}

static inline
void prop_validate(SEXP property, SEXP value, SEXP object) {

  static SEXP prop_validate = NULL;
  if (prop_validate == NULL)
    prop_validate = Rf_findVarInFrame(ns_S7, Rf_install("prop_validate"));

  SEXP errmsg = eval_here(Rf_lang4(prop_validate, property, value, object));
  if (errmsg != R_NilValue) signal_error(errmsg);
}

static inline
void obj_validate(SEXP object) {
  static SEXP validate = NULL;
  if (validate == NULL)
    validate = Rf_findVarInFrame(ns_S7, Rf_install("validate"));

  eval_here(Rf_lang4(
    validate, object,
    /* recursive = */ Rf_ScalarLogical(TRUE),
    /* properties = */ Rf_ScalarLogical(FALSE)));
}

SEXP prop_set_(SEXP object, SEXP name, SEXP check_sexp, SEXP value) {

  check_is_S7(object);

  SEXP name_rchar = STRING_ELT(name, 0);
  const char *name_char = CHAR(name_rchar);
  SEXP name_sym = Rf_installTrChar(name_rchar);

  Rboolean check = Rf_asLogical(check_sexp);
  Rboolean should_validate_obj = check;
  Rboolean should_validate_prop = check;

  SEXP S7_class = Rf_getAttrib(object, sym_S7_class);
  SEXP properties = Rf_getAttrib(S7_class, sym_properties);
  SEXP property = extract_name(properties, name_char);

  if (property == R_NilValue)
    signal_prop_error_unknown(object, name);

  SEXP setter = extract_name(property, "setter");
  SEXP getter = extract_name(property, "getter");

  if (getter != R_NilValue && setter == R_NilValue)
    signal_prop_error("Can't set read-only property %s@%s", object, name);

  PROTECT_INDEX object_pi;
  object = Rf_shallow_duplicate(object);
  PROTECT_WITH_INDEX(object, &object_pi);

  if (setter_callable_no_recurse(setter, object, name_sym, &should_validate_obj)) {
    // use setter()
    object = eval_here(Rf_lang3(setter, object, value));
    REPROTECT(object, ipx);
    setter_no_recurse_clear(object, name_sym);
  } else {
    // don't use setter()
    if (should_validate_prop)
      prop_validate(property, value, object);
    Rf_setAttrib(object, name_sym, value);
  }

  if (should_validate_obj)
    obj_validate(object);

  UNPROTECT(1);
  return object;
}
