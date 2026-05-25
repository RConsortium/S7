#include "compat.h"
#include <string.h>

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

extern SEXP R_FALSE;

static inline
SEXP eval_here(SEXP lang) {
  PROTECT(lang);
  SEXP ans = Rf_eval(lang, ns_S7);
  UNPROTECT(1);
  return ans;
}

static inline
SEXP ns_get(const char* name) {
  SEXP val = s7_get_var_in_frame(ns_S7, Rf_install(name), NULL);
  if (val == NULL)
    Rf_error("Can't find `%s` in the S7 namespace", name);
  return val;
}

static __attribute__((noreturn))
void signal_is_not_S7(SEXP object) {
  static SEXP check_is_S7 = NULL;
  if (check_is_S7 == NULL)
    check_is_S7 = ns_get("check_is_S7");

  // will signal error
  eval_here(Rf_lang2(check_is_S7, object));
  while(1);
}


static __attribute__((noreturn))
void signal_prop_error(const char* fmt, SEXP object, SEXP name) {
  static SEXP signal_prop_error = NULL;
  if (signal_prop_error == NULL)
    signal_prop_error = ns_get("signal_prop_error");

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
    signal_error = ns_get("signal_error");

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

enum prop_accessor {
  PROP_GETTER,
  PROP_SETTER
};

static inline
SEXP prop_accessor_marker(enum prop_accessor accessor) {
  switch (accessor) {
  case PROP_GETTER:
    return sym_dot_getting_prop;
  case PROP_SETTER:
    return sym_dot_setting_prop;
  }

  Rf_error("Internal error: unknown property accessor kind");
  return R_NilValue;
}

static inline
Rboolean setter_callable_no_recurse(SEXP setter, SEXP object, SEXP name_sym,
                                    Rboolean* should_validate_obj) {
  // Check if we should call `setter` and if so, prepare `setter` for calling.

  SEXP marker_sym = prop_accessor_marker(PROP_SETTER);
  SEXP no_recurse_pairlist = Rf_getAttrib(object, marker_sym);
  if (TYPEOF(no_recurse_pairlist) == LISTSXP) {
    // if there is a 'no_recurse' list, then this is not the top-most prop<-
    // call for this object, i.e, we're currently evaluating a `prop<-` call
    // called from within a custom property setter. We should only call
    // validate(object) once from the top-most prop<- call, after the last
    // custom setter() has returned.
    *should_validate_obj = FALSE;
    if (pairlist_contains(no_recurse_pairlist, name_sym))
      return FALSE;
  }

  if (TYPEOF(setter) != CLOSXP)
    return FALSE; // setter not callable

  Rf_setAttrib(object, marker_sym, Rf_cons(name_sym, no_recurse_pairlist));
  return TRUE; // object is now now marked non-recursive for this property setter, safe to call

  // optimization opportunity: combine the actions of getAttrib()/setAttrib()
  // into one loop, so we can avoid iterating over ATTRIB(object) twice.
}

static inline
void accessor_no_recurse_clear_if_present(SEXP object, SEXP name_sym,
                                          enum prop_accessor accessor) {
  SEXP marker_sym = prop_accessor_marker(accessor);
  SEXP list = Rf_getAttrib(object, marker_sym);
  if (TYPEOF(list) != LISTSXP)
    return;

  SEXP prev = R_NilValue;
  for (SEXP node = list; node != R_NilValue; prev = node, node = CDR(node)) {
    if (CAR(node) != name_sym)
      continue;

    SEXP rest = CDR(node);
    if (prev == R_NilValue)
      Rf_setAttrib(object, marker_sym, rest);
    else
      SETCDR(prev, rest);

    return;
  }
}

static SEXP prop_call_env = NULL;

void prop_init(void) {
  if (prop_call_env != NULL)
    Rf_error("Internal error: property call environment is already initialized");

  SEXP call = PROTECT(Rf_lang3(Rf_install("new.env"), R_FALSE, ns_S7));
  prop_call_env = Rf_eval(call, R_BaseEnv);
  R_PreserveObject(prop_call_env);
  UNPROTECT(1);
}

static SEXP prop_call_symbol(SEXP S7_class, SEXP name) {
  SEXP class_name = Rf_getAttrib(S7_class, sym_name);
  if (TYPEOF(class_name) != STRSXP || Rf_length(class_name) != 1)
    Rf_error("Internal error: S7 class name must be a string");

  SEXP class_name_rchar = STRING_ELT(class_name, 0);
  SEXP name_rchar = STRING_ELT(name, 0);
  const char* class_name_char = CHAR(class_name_rchar);
  const char* name_char = CHAR(name_rchar);

  int class_name_len = LENGTH(class_name_rchar);
  int name_len = LENGTH(name_rchar);
  int call_name_len = class_name_len + name_len + 3;
  char* call_name = R_alloc(call_name_len + 1, sizeof(char));

  call_name[0] = '<';
  memcpy(call_name + 1, class_name_char, class_name_len);
  call_name[class_name_len + 1] = '>';
  call_name[class_name_len + 2] = '@';
  memcpy(call_name + class_name_len + 3, name_char, name_len + 1);

  SEXP call_name_rchar = PROTECT(
      Rf_mkCharLenCE(call_name, call_name_len, CE_NATIVE));
  SEXP call_sym = Rf_installTrChar(call_name_rchar);
  UNPROTECT(1);

  return call_sym;
}

struct prop_call_data {
  SEXP call;
  SEXP fn_sym;
  SEXP marked_object;
  SEXP property_sym;
  SEXP result;
  enum prop_accessor accessor;
};

static SEXP prop_call_eval(void* data) {
  struct prop_call_data* call_data = (struct prop_call_data*) data;
  call_data->result = Rf_eval(call_data->call, prop_call_env);
  return call_data->result;
}

static void prop_call_cleanup(void* data, Rboolean jump) {
  struct prop_call_data* call_data = (struct prop_call_data*) data;

  s7_clear_var_in_frame(prop_call_env, call_data->fn_sym);

  SEXP object = call_data->marked_object;
  if (!jump && call_data->accessor == PROP_SETTER)
    object = call_data->result;

  accessor_no_recurse_clear_if_present(
      object, call_data->property_sym, call_data->accessor);
}

static inline
SEXP do_getter_call(SEXP getter, SEXP S7_class, SEXP name, SEXP object,
                    SEXP name_sym) {
  int n_protected = 0;
  SEXP fn_sym = prop_call_symbol(S7_class, name);
  SEXP marked_object = object;

  Rf_defineVar(fn_sym, getter, prop_call_env);

  switch (TYPEOF(object)) {
  case LANGSXP:
  case SYMSXP:
    object = PROTECT(Rf_lang2(fn_base_quote, object));
    ++n_protected;
  }

  SEXP call = PROTECT(Rf_lang2(fn_sym, object));
  ++n_protected;

  struct prop_call_data call_data = {
    call,
    fn_sym,
    marked_object,
    name_sym,
    R_NilValue,
    PROP_GETTER
  };

  SEXP result = R_UnwindProtect(
      prop_call_eval, &call_data,
      prop_call_cleanup, &call_data,
      NULL);

  UNPROTECT(n_protected);
  return result;
}

static inline
SEXP do_setter_call(SEXP setter, SEXP S7_class, SEXP name, SEXP object,
                    SEXP name_sym, SEXP value) {
  int n_protected = 0;
  SEXP fn_sym = prop_call_symbol(S7_class, name);
  SEXP marked_object = object;

  Rf_defineVar(fn_sym, setter, prop_call_env);

  switch (TYPEOF(object)) {
  case LANGSXP:
  case SYMSXP:
    object = PROTECT(Rf_lang2(fn_base_quote, object));
    ++n_protected;
  }

  switch (TYPEOF(value)) {
  case LANGSXP:
  case SYMSXP:
    value = PROTECT(Rf_lang2(fn_base_quote, value));
    ++n_protected;
  }

  SEXP call = PROTECT(Rf_lang3(fn_sym, object, value));
  ++n_protected;

  struct prop_call_data call_data = {
    call,
    fn_sym,
    marked_object,
    name_sym,
    R_NilValue,
    PROP_SETTER
  };

  SEXP result = R_UnwindProtect(
      prop_call_eval, &call_data,
      prop_call_cleanup, &call_data,
      NULL);

  UNPROTECT(n_protected);
  return result;
}

static inline
void prop_validate(SEXP property, SEXP value, SEXP object) {

  static SEXP prop_validate = NULL;
  if (prop_validate == NULL)
    prop_validate = ns_get("prop_validate");

  SEXP errmsg = eval_here(Rf_lang4(prop_validate, property, value, object));
  if (errmsg != R_NilValue) signal_error(errmsg);
}

static inline
void obj_validate(SEXP object) {
  static SEXP validate = NULL;
  if (validate == NULL)
    validate = ns_get("validate");

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

  SEXP marker_sym = prop_accessor_marker(PROP_GETTER);
  SEXP no_recurse_pairlist = Rf_getAttrib(object, marker_sym);
  if (TYPEOF(no_recurse_pairlist) == LISTSXP &&
      pairlist_contains(no_recurse_pairlist, name_sym))
    return FALSE;

  Rf_setAttrib(object, marker_sym, Rf_cons(name_sym, no_recurse_pairlist));
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

    SEXP value = PROTECT(
        do_getter_call(getter, S7_class, name, object, name_sym));
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
    REPROTECT(
        object = do_setter_call(
            setter,
            S7_class,
            name,
            object,
            name_sym,
            value),
        object_pi);
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
