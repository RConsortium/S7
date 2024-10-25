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
extern SEXP sym_dot_getting_prop;
extern SEXP sym_dot_setting_prop;

extern SEXP fn_base_quote;

static inline
SEXP eval_here(SEXP lang) {
  PROTECT(lang);
  SEXP ans = Rf_eval(lang, ns_S7);
  UNPROTECT(1);
  return ans;
}

static inline
SEXP do_call1(SEXP fn, SEXP arg) {
  SEXP call, answer;
  switch (TYPEOF(arg)) {
  case LANGSXP:
  case SYMSXP:
    arg = PROTECT(Rf_lang2(fn_base_quote, arg));
    call = PROTECT(Rf_lang2(fn, arg));
    answer = Rf_eval(call, ns_S7);
    UNPROTECT(2);
    return answer;
  default:
    call = PROTECT(Rf_lang2(fn, arg));
    answer = Rf_eval(call, ns_S7);
    UNPROTECT(1);
    return answer;
  }
}

static inline SEXP do_call2(SEXP fn, SEXP arg1, SEXP arg2) {
  int n_protected = 0;
  // Protect the arguments from evaluation if they are SYMSXP or LANGSXP
  switch (TYPEOF(arg1)) {
  case LANGSXP:
  case SYMSXP:
    arg1 = PROTECT(Rf_lang2(fn_base_quote, arg1));
    ++n_protected;
  }

  switch (TYPEOF(arg2)) {
  case LANGSXP:
  case SYMSXP:
    arg2 = PROTECT(Rf_lang2(fn_base_quote, arg2));
    ++n_protected;
  }

  SEXP call = PROTECT(Rf_lang3(fn, arg1, arg2));
  ++n_protected;

  SEXP result = Rf_eval(call, ns_S7);

  UNPROTECT(n_protected);
  return result;
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

  if (TYPEOF(names) == STRSXP) {
    for (int i = 0, n = Rf_length(names); i < n; i++) {
      if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0)
        return i;
    }
  }
  return -1;
}

static inline
SEXP extract_name(SEXP list, const char* name) {
  int i = name_idx(list, name);
  return i == -1 ? R_NilValue : VECTOR_ELT(list, i);
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
  // Check if we should call `setter` and if so, prepare `setter` for calling.

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
    return TRUE; // object is now now marked non-recursive for this property setter, safe to call

  // optimization opportunity: combine the actions of getAttrib()/setAttrib()
  // into one loop, so we can avoid iterating over ATTRIB(object) twice.
}

static inline
void accessor_no_recurse_clear(SEXP object, SEXP name_sym, SEXP no_recurse_list_sym) {
  SEXP list = Rf_getAttrib(object, no_recurse_list_sym);
  list = pairlist_remove(list, name_sym);
  Rf_setAttrib(object, no_recurse_list_sym, list);

  // optimization opportunity: same as setter_callable_no_recurse
}

#define getter_no_recurse_clear(...) \
    accessor_no_recurse_clear(__VA_ARGS__, sym_dot_getting_prop)

#define setter_no_recurse_clear(...) \
    accessor_no_recurse_clear(__VA_ARGS__, sym_dot_setting_prop)

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

  switch (TYPEOF(object)) {
  case LANGSXP:
  case SYMSXP: {
    // Wrap the call or symbol in quote(), so it doesn't evaluate in Rf_eval()
    object = PROTECT(Rf_lang2(fn_base_quote, object));
    eval_here(Rf_lang4(validate, object,
                       /* recursive = */ Rf_ScalarLogical(TRUE),
                       /* properties = */ Rf_ScalarLogical(FALSE)));
    UNPROTECT(1); // object
    return;
  }

  default:
    eval_here(Rf_lang4(
        validate, object,
        /* recursive = */ Rf_ScalarLogical(TRUE),
        /* properties = */ Rf_ScalarLogical(FALSE)));
  }
}

static inline
Rboolean getter_callable_no_recurse(SEXP getter, SEXP object, SEXP name_sym) {
  // Check if we should call getter and if so, prepare object for calling the getter.

  SEXP no_recurse_list = Rf_getAttrib(object, sym_dot_getting_prop);
  if (TYPEOF(no_recurse_list) == LISTSXP &&
      pairlist_contains(no_recurse_list, name_sym))
    return FALSE;

  Rf_setAttrib(object, sym_dot_getting_prop,
               Rf_cons(name_sym, no_recurse_list));
  return TRUE; // object is now now marked non-recursive for this property accessor, safe to call

  // optimization opportunity: combine the actions of getAttrib()/setAttrib()
  // into one loop, so we can avoid iterating over ATTRIB(object) twice.
}


SEXP prop_(SEXP object, SEXP name) {
  check_is_S7(object);

  SEXP name_rchar = STRING_ELT(name, 0);
  const char* name_char = CHAR(name_rchar);
  SEXP name_sym = Rf_installTrChar(name_rchar);

  SEXP S7_class = Rf_getAttrib(object, sym_S7_class);
  SEXP properties = Rf_getAttrib(S7_class, sym_properties);

  // try getter() if appropriate
  SEXP property = extract_name(properties, name_char);
  SEXP getter = extract_name(property, "getter");
  if (TYPEOF(getter) == CLOSXP &&
      getter_callable_no_recurse(getter, object, name_sym)) {

    SEXP value = PROTECT(do_call1(getter, object));
    getter_no_recurse_clear(object, name_sym);
    UNPROTECT(1); // value
    return value;
  }

  // try to resolve property from the object attributes
  SEXP value = Rf_getAttrib(object, name_sym);

  // This is commented out because we currently have no way to distinguish between
  // a prop with a value of NULL, and a prop value that is unset/missing.
  // // fall back to fetching the default property value from the object class
  // if (value == R_NilValue)
  //   value = extract_name(property, "default");

  // validate that we're accessing a valid property
  if (property != R_NilValue)
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

  signal_prop_error_unknown(object, name);
  return R_NilValue; // unreachable, for compiler
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
  // maybe use R_shallow_duplicate_attr() here instead
  // once it becomes API or S7 becomes part of R
  object = Rf_shallow_duplicate(object);
  PROTECT_WITH_INDEX(object, &object_pi);

  if (setter_callable_no_recurse(setter, object, name_sym, &should_validate_obj)) {
    // use setter()
    REPROTECT(object = do_call2(setter, object, value), object_pi);
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
